use std::{
    collections::HashSet,
    fs::File,
    io::{Read, Write},
    rc::Rc,
};

use dependency_graph::{DependencyGraph, Step};
use roxmltree::Node;

trait KIString {
    fn as_safe(&self) -> String;

    fn get_namespace(&self) -> Option<String>;

    fn remove_namespace(&self) -> String;

    fn remove_sharedptr(&self) -> String;

    fn remove_duplicate_class(&self) -> String;
}

impl KIString for String {
    fn as_safe(&self) -> String {
        self.replace(
            "std::basic_string<char,std::char_traits<char,std::allocator<char> >",
            "std::string",
        )
        .replace(
            "std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >",
            "std::wstring",
        )
        .replace("class ", "")
        .replace("struct ", "")
        .replace("enum ", "")
        .replace("union ", "")
        .replace("unsigned __int64", "uint64_t")
    }

    fn get_namespace(&self) -> Option<String> {
        if self.contains("::") && !self.contains("std::") {
            return Some(self[..self.find("::").unwrap()].to_string());
        }
        None
    }

    fn remove_namespace(&self) -> String {
        if let Some(namespace) = self.get_namespace() {
            return self.replace(&format!("{}::", namespace), "");
        }
        self.to_string()
    }

    // messes with dep graph
    fn remove_sharedptr(&self) -> String {
        if self.contains("SharedPointer") {
            return self.replace("SharedPointer<", "").replace(">", "");
        }
        self.to_string()
    }

    fn remove_duplicate_class(&self) -> String {
        if self.contains("class class ") {
            return self.replace("class class ", "class ");
        }
        self.to_string()
    }
}

#[derive(Debug, Clone)]
struct EnumValue {
    name: String,
    value: String,
}

#[derive(Debug, Clone)]
struct EnumProperty {
    name: String,
    values: Vec<EnumValue>,
}

#[derive(Debug, Clone)]
struct ClassProperty {
    property_name: String,           // property name
    property_type: String,           // property type
    property_note: String,           // comment, only seems to be for some gui elements
    offset: String,                  // offset of property from class base
    container_type: String,          // the container that holds the value
    enum_info: Option<EnumProperty>, // optional enum information
}

impl ClassProperty {
    fn new(node: Node) -> ClassProperty {
        let property_type = node.attribute("Type").unwrap().to_string();

        let is_enum = property_type.contains("enum")
            && node.first_element_child().map_or(false, |child| {
                child
                    .attribute("Name")
                    .map_or(false, |attr| attr.contains("enum"))
            });

        let enum_info = if is_enum {
            let first_child = node.first_element_child().unwrap();
            let enum_name = first_child.attribute("Name").unwrap().to_string();
            let mut enum_values = vec![];

            let mut cur = first_child.first_element_child().unwrap();
            while let Some(next_sibling) = cur.next_sibling_element() {
                let e_name = cur.attribute("Name").unwrap().to_string().replace(" ", "");
                if e_name != "__DEFAULT" {
                    enum_values.push(EnumValue {
                        name: e_name,
                        value: cur.attribute("Value").unwrap().to_string(),
                    });
                }
                cur = next_sibling;
            }

            Some(EnumProperty {
                name: enum_name,
                values: enum_values,
            })
        } else {
            None
        };

        ClassProperty {
            property_name: node.attribute("Name").unwrap().to_string(),
            property_type,
            property_note: node.attribute("Note").unwrap().to_string(),
            offset: node.attribute("Offset").unwrap().to_string(),
            container_type: match node.attribute("Container").unwrap().to_string().as_str() {
                "Vector" => String::from("std::vector"),
                "List" => String::from("std::list"),
                "Static" => String::from(""),
                s => {
                    println!("Unknown container {}", s);
                    String::from("")
                }
            },
            enum_info,
        }
    }

    fn is_enum(&self) -> bool {
        self.enum_info.is_some()
    }

    fn serialize(self) -> String {
        let mut ret = String::from("");
        if let Some(enum_info) = self.enum_info {
            ret += &format!("enum {} {{\n", enum_info.name.as_safe().remove_namespace());
            for enum_elem in enum_info.values {
                ret += &format!(
                    "          {} = {},\n",
                    enum_elem
                        .name
                        .as_safe()
                        .remove_namespace()
                        .replace("-", "_"),
                    enum_elem.value
                );
            }
            ret += &format!("     }};\n");
        } else {
            ret += &format!(
                "{}{}{} {};{}\n",
                if !self.container_type.is_empty() {
                    format!("{}<", self.container_type)
                } else {
                    String::from("")
                },
                self.property_type.as_safe(),
                if !self.container_type.is_empty() {
                    String::from(">")
                } else {
                    String::from("")
                },
                self.property_name.as_safe().replace(".", "_"),
                if self.property_note == self.property_name {
                    String::from("")
                } else {
                    format!(" // {}", &self.property_note)
                }
            );
        }
        ret
    }
}

#[derive(Debug, Clone)]
struct ClassFunction {
    function_name: String,
    return_type: String,
    arg_count: i32,
    arg_types: Vec<String>,
}

impl ClassFunction {
    fn new(node: Node) -> ClassFunction {
        let function_name = node.attribute("Name").unwrap().to_string();
        let return_type = node.attribute("ReturnType").unwrap().to_string();
        let arg_count = node
            .attribute("ArgumentCount")
            .unwrap()
            .parse::<i32>()
            .unwrap();

        let arg_types = (0..arg_count)
            .map(|i| {
                node.attribute(format!("Argument{}", i).as_str())
                    .unwrap()
                    .to_string()
            })
            .collect();

        ClassFunction {
            function_name,
            return_type,
            arg_count,
            arg_types,
        }
    }

    fn serialize(&self) -> String {
        let mut ret = format!(
            "virtual {} {}(",
            self.return_type.as_safe(),
            self.function_name
        );
        for i in 0..self.arg_count {
            let name = format!("arg{}", i.to_string());
            ret += &format!(
                "{} {}{}",
                &self.arg_types[0].as_safe(),
                name,
                if i == self.arg_count - 1 { "" } else { ", " }
            )
        }
        ret += ");\n";
        ret
    }
}

#[derive(Debug, Clone)]
enum ClassTypeT {
    CLASS,
    STRUCT,
}
#[derive(Debug, Clone)]
struct WizClass {
    class_type: ClassTypeT,
    class_name: String,
    base_class_name: Option<String>,
    base_class_idx: Option<usize>, // index in all_classes vec of this classes base class
    size: String,
    properties: Vec<ClassProperty>,
    functions: Vec<ClassFunction>,
    sub_classes: Vec<WizClass>,
    namespace: Option<String>,
    is_subclass: bool,
}

impl WizClass {
    fn new(node: Node) -> WizClass {
        let class_name = node.attribute("Name").unwrap().to_string();

        let mut properties: Vec<ClassProperty> = node
            .children()
            .filter(|child| child.tag_name().name() == "Property")
            .map(ClassProperty::new)
            .collect();

        properties.sort_by_key(|prop| prop.offset.parse::<i32>().unwrap_or(0));

        let functions = node
            .children()
            .filter(|child| child.tag_name().name() == "Function")
            .map(ClassFunction::new)
            .collect();

        let class_type = if class_name.contains("class") {
            ClassTypeT::CLASS
        } else {
            ClassTypeT::STRUCT
        };

        WizClass {
            class_type,
            class_name,
            base_class_name: if let Some(base_class_node) = node.attribute("Base") {
                Some(base_class_node.to_string())
            } else {
                None
            },
            base_class_idx: None,
            size: node.attribute("Size").unwrap().to_string(),
            properties,
            functions,
            sub_classes: vec![],
            namespace: None,
            is_subclass: false,
        }
    }

    fn get_parent_name(&self) -> Option<String> {
        let safe = self.class_name.clone();
        if safe.contains("::") && !safe.contains("std::") {
            Some(safe[..safe.find("::").unwrap()].to_string())
        } else {
            None
        }
    }

    fn get_parent<'a>(&self, classes: &'a mut [WizClass]) -> Option<&'a mut WizClass> {
        let parent_name = self.get_parent_name();
        if let Some(p_name) = parent_name {
            classes.iter_mut().find(|c| c.class_name == p_name)
        } else {
            None
        }
    }

    fn is_empty(&self) -> bool {
        /*let mut empty = self.functions.is_empty() && self.sub_classes.is_empty();
        for prop in &self.properties {
            if prop.is_enum() {
                empty = false;
            }
        }*/
        let mut empty =
            self.functions.is_empty() && self.sub_classes.is_empty() && self.properties.is_empty();
        return empty;
    }

    fn serialize(&self, sub_class: bool) -> String {
        let type_str = match self.class_type {
            ClassTypeT::CLASS => "class",
            ClassTypeT::STRUCT => "struct",
        };
        let mut real_name = if let Some(namespace) = &self.namespace {
            self.class_name
                .as_safe()
                .replace(&format!("{}::", namespace.as_safe()), "")
        } else {
            self.class_name.as_safe()
        };

        if sub_class {
            real_name = real_name.remove_namespace();
        }

        let mut class_def = if let Some(namespace) = &self.namespace {
            format!("namespace {}{{\n", namespace.as_safe())
        } else {
            String::from("")
        };

        if self.base_class_name.is_none() {
            class_def += &format!(
                "{}{} {} {{{}",
                if sub_class {
                    String::from("     ")
                } else {
                    String::from("")
                },
                type_str,
                real_name,
                if self.is_empty() {
                    String::from("")
                } else {
                    String::from("\n")
                }
            );
        } else {
            class_def += &format!(
                "{}{} {} : public {} {{{}",
                if sub_class {
                    String::from("     ")
                } else {
                    String::from("")
                },
                type_str,
                real_name,
                self.base_class_name.as_ref().unwrap().as_safe().clone(),
                if self.is_empty() {
                    String::from("")
                } else {
                    String::from("\n")
                }
            );
        }

        class_def += &format!(
            "{}",
            if self.is_empty() {
                String::from("")
            } else {
                String::from("public:\n")
            }
        );

        for sub_class in &self.sub_classes {
            class_def += &sub_class.serialize(true);
        }

        for property in &self.properties {
            //if property.is_enum() {
            class_def += &format!("     {}", property.clone().serialize());
            //}
        }

        for function in &self.functions {
            class_def += &format!("     {}", function.clone().serialize());
        }

        class_def += &format!("}};\n",);

        if self.namespace.is_some() {
            class_def += "}\n";
        }

        class_def
    }
}

struct ClassesContainer {
    classes: Vec<WizClass>,
    // will need an enum lookup
}

impl ClassesContainer {
    fn new(root_node: Node) -> ClassesContainer {
        ClassesContainer {
            classes: root_node
                .descendants()
                .enumerate()
                .filter_map(|(_index, node)| {
                    (node.tag_name().name() == "Class").then(|| WizClass::new(node))
                })
                .collect::<Vec<_>>(),
        }
    }

    fn populate_subclasses_and_namespaces(&mut self) {
        let cloned = self.classes.clone();
        for i in (0..self.classes.len()).rev() {
            let class = &cloned[i];
            if let Some(parent) = class.get_parent(&mut self.classes) {
                parent.sub_classes.push(class.clone());
                println!(
                    "added {} as a subclass of {}",
                    class.class_name, parent.class_name
                );
                self.classes[i].is_subclass = true;
                //self.classes.remove(i);
            }

            if let Some(parent_name) = class.get_parent_name() {
                if class.get_parent(&mut self.classes).is_none() {
                    self.classes[i].namespace = Some(parent_name);
                }
            }
        }
    }

    fn populate_base_class_indices(&mut self) {
        for class_index in 0..self.classes.len() {
            let base_class_name = self.classes[class_index]
                .base_class_name
                .clone()
                .unwrap_or_default();
            if let Some(base_class_index) = self
                .classes
                .iter()
                .position(|p_class| p_class.class_name == base_class_name)
            {
                self.classes[class_index].base_class_idx = Some(base_class_index);
            }
        }
    }

    // filter duplicate properties and function classes.as_safe()
    fn filter_shared_data(&mut self) {
        let non_mut = self.classes.clone();
        for class in self.classes.iter_mut() {
            if let Some(base_class_index) = class.base_class_idx {
                let b_class = &non_mut[base_class_index];
                class.functions.retain(|func| {
                    !b_class
                        .functions
                        .iter()
                        .any(|func_p| func.function_name == func_p.function_name)
                });
                class.properties.retain(|prop| {
                    !b_class
                        .properties
                        .iter()
                        .any(|prop_p| prop.property_name == prop_p.property_name)
                });
            }
        }
    }

    fn find_by_name(&self, name: String) -> Option<WizClass> {
        if let Some(element) = self
            .classes
            .iter()
            .find(|package| package.class_name == name)
        {
            Some(element.clone())
        } else {
            None
        }
    }
}

#[derive(Debug, Clone)]
struct Dependency {
    name: String,
}

#[derive(Debug, Clone)]
struct Package {
    name: String,
    dependencies: Vec<Dependency>,
}

fn collect_deps(class: &WizClass) -> Vec<Dependency> {
    if class.is_subclass {
        return vec![];
    }

    let mut dependencies = HashSet::new();

    if let Some(base_class_name) = &class.base_class_name {
        if let Some(namespace) = base_class_name.get_namespace() {
            // we need to check if namespace exists before inserting namespace as dep, if not insert the else clause
            dependencies.insert(namespace);
        } else {
            dependencies.insert(
                base_class_name
                    .to_string()
                    .replace("*", "")
                    .remove_sharedptr(),
            );
        }
    }

    for prop in &class.properties {
        if !prop.property_type.is_empty() {
            if let Some(namespace) = prop.property_type.get_namespace() {
                // we need to check if namespace exists before inserting namespace as dep, if not insert the else clause
                dependencies.insert(namespace);
            } else {
                dependencies.insert(
                    prop.property_type
                        .to_string()
                        .replace("*", "")
                        .remove_sharedptr()
                        .remove_duplicate_class(),
                );
            }
        }
    }

    for func in &class.functions {
        if !func.return_type.is_empty() {
            if let Some(namespace) = func.return_type.get_namespace() {
                // we need to check if namespace exists before inserting namespace as dep, if not insert the else clause
                dependencies.insert(namespace);
            } else {
                dependencies.insert(
                    func.return_type
                        .clone()
                        .replace("*", "")
                        .remove_sharedptr()
                        .remove_duplicate_class(),
                );
            }
        }

        for arg in &func.arg_types {
            if !arg.is_empty() {
                if let Some(namespace) = arg.get_namespace() {
                    dependencies.insert(namespace);
                } else {
                    dependencies.insert(
                        arg.clone()
                            .replace("*", "")
                            .remove_sharedptr()
                            .remove_duplicate_class(),
                    );
                }
            }
        }
    }

    for subclass in &class.sub_classes {
        let subclass_deps = collect_deps(subclass);
        let subclass_dep_names = subclass_deps.into_iter().map(|dep| dep.name);
        dependencies.extend(subclass_dep_names);
    }

    let mut unique_dependencies = dependencies.into_iter().collect::<Vec<_>>();
    unique_dependencies.sort();

    unique_dependencies
        .into_iter()
        .map(|name| Dependency { name })
        .collect()
}

impl Package {
    fn from(class: &WizClass) -> Package {
        Package {
            name: class.class_name.clone(),
            dependencies: collect_deps(class),
        }
    }
}

impl dependency_graph::Node for Package {
    type DependencyType = Dependency;

    fn dependencies(&self) -> &[Self::DependencyType] {
        &self.dependencies[..]
    }

    fn matches(&self, dependency: &Self::DependencyType) -> bool {
        self.name == dependency.name
    }
}

async fn dump_typedefs() -> Result<String, std::io::Error> {
    let mut res: Vec<u8> = Vec::new();
    let mut f =
        File::open("/home/binarybandit/Desktop/Wizard101Launcher/test/Bin/WizardClientDefs.xml")?;
    f.read_to_end(&mut res).expect("whoops");
    Ok(String::from_utf8_lossy(&res).to_string())
}

pub async fn dump_wiz_classes() {
    println!("Dumping typedefs..");

    let xml_dump = dump_typedefs().await.unwrap();
    let doc = roxmltree::Document::parse(&xml_dump).unwrap();
    let root_node = doc.root().first_child().unwrap();

    let mut classes = ClassesContainer::new(root_node);
    classes.populate_base_class_indices();
    classes.filter_shared_data();
    classes.populate_subclasses_and_namespaces();
    // classes.populate_base_class_indices(); // have to do this twice as subclasses change the base class index, subclasses won't have correct index, but shouldn't be needed anymore

    let mut packages: Vec<Package> = classes
        .classes
        .iter()
        .cloned()
        .filter(|class| if class.is_subclass { false } else { true })
        .collect::<Vec<_>>()
        .iter()
        .map(Package::from)
        .collect();

    for package in packages.iter_mut() {
        package
            .dependencies
            .retain(|dependency| dependency.name != package.name);
    }

    let cloned = packages.clone();

    /*let names_with_deps: Vec<&String> = cloned
        .iter()
        .flat_map(|package| package.dependencies.iter().map(|dep| &dep.name))
        .collect();
    packages.retain(|package| names_with_deps.contains(&&package.name));*/

    let graph = DependencyGraph::from(&packages[..]);

    let mut ret = String::from(
        "#include <string>
#include <memory>
#include <cstdint>
#include <vector>
#include <list>

/* BEGIN OVERRIDES */
/* END OVERRIDES */

/* PROPERTYCLASS TYPE DEFINITIONS */
using gid = uint64_t;

using SerializedBuffer = void*;

using Euler = void*; // what??

template <class T>
using SharedPointer = std::shared_ptr<T>;

template <typename T>
struct Rect {
    T top, left, right, bottom;
};

template <typename T>
struct Point {
    T x, y;
};

template <typename T>
struct Size {
    T val;
};

struct Matrix3x3 {
    float i[3];
    float j[3];
    float k[3];
};

struct SimpleFace {
    int vertexIndices[3]; // Assuming triangular faces
};

struct SimpleVert {
    float x, y, z;
};

struct Vector3D {
    float x, y, z;
};

struct Color {
    float b, r, g, a;
};

struct s24 {
    unsigned int data : 24;
};

struct bui2 {
    unsigned int data : 2;
};

struct bui4 {
    unsigned int data : 4;
};

struct bui5 {
    unsigned int data : 5;
};

struct bui7 {
    unsigned int data : 7;
};

/* START PROPERTYCLASSES */\n",
    );
    let mut serialized_classes = HashSet::new(); // Track serialized classes
    for package in graph {
        match package {
            Step::Resolved(package) => {
                // Check if the class has already been serialized
                if serialized_classes.contains(&package.name) {
                    continue; // Skip serialization
                }

                if let Some(found_class) = classes
                    .classes
                    .iter()
                    .find(|class| class.class_name == package.name)
                {
                    ret += &format!("{}\n", found_class.serialize(false));
                    serialized_classes.insert(package.name.clone());
                } else {
                    println!("Couldn't find class during serialization: {}", package.name);
                }
            }
            Step::Unresolved(_) => {}
        }
    }

    let mut class_file = File::create("classes.cpp").unwrap();
    class_file.write_all(ret.as_bytes()).unwrap();

    let mut file = File::create("graph").unwrap();
    file.write_all(&format!("{:#?}\n", packages).as_bytes());
}
