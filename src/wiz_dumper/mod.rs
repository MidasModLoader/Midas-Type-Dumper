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
    fn force_remove_namespace(&self) -> String;

    fn remove_sharedptr(&self) -> String;

    fn remove_duplicate_class(&self) -> String;

    fn replace_missing(&self) -> String;
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

    fn force_remove_namespace(&self) -> String {
        if self.contains("::") && !self.contains("std::") {
            return self[self.find("::").unwrap() + 2..self.len()].to_string();
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

    fn replace_missing(&self) -> String {
        self.replace("BattlegroundPlayerStats::PlayerTeamEnum", "MISSING_TYPE")
            .replace("eGender", "MISSING_TYPE")
            .replace("PhysicsSimMass::CylinderDirection", "MISSING_TYPE")
            .replace("ProxyType", "BoxGeomParams::ProxyType")
            .replace("DoodleDoug::DdDirection", "MISSING_TYPE")
            .replace("CrownShopViews::OnSelectCallbackArg", "MISSING_TYPE")
            .replace("SegmentationRequrinment::OPERATOR_TYPE", "MISSING_TYPE")
            .replace(
                "DynamicSigilSymbol",
                "DynamicSigilSubcircle::DynamicSigilSymbol",
            )
    }
}

fn capitalize_first(s: &str) -> String {
    let mut c = s.chars();
    match c.next() {
        None => String::new(),
        Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
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

        let mut encountered_names = HashSet::new();

        let enum_info = if is_enum {
            let first_child = node.first_element_child().unwrap();
            let enum_name = first_child.attribute("Name").unwrap().to_string();
            let mut enum_values = vec![];

            let mut cur = first_child.first_element_child().unwrap();
            while let Some(next_sibling) = cur.next_sibling_element() {
                let e_name = cur.attribute("Name").unwrap().to_string().replace(" ", "");
                if e_name != "__DEFAULT" && !encountered_names.contains(&e_name) {
                    enum_values.push(EnumValue {
                        name: e_name.clone(),
                        value: cur.attribute("Value").unwrap().to_string(),
                    });
                    encountered_names.insert(e_name);
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
            property_name: node
                .attribute("Name")
                .unwrap()
                .to_string()
                .replace("m_", ""),
            property_type,
            property_note: node
                .attribute("Note")
                .unwrap()
                .to_string()
                .replace("m_", ""),
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
            let full_type = format!(
                "{}{}{}",
                if !self.container_type.is_empty() {
                    format!("{}<", self.container_type)
                } else {
                    String::from("")
                },
                self.property_type.as_safe().replace_missing(),
                if !self.container_type.is_empty() {
                    String::from(">")
                } else {
                    String::from("")
                }
            );

            let mut safe_name = self.property_name.as_safe().replace(".", "_");
            if safe_name == "default" {
                safe_name = "default_".to_owned();
            }

            if safe_name == "Color" {
                safe_name = "color".to_owned();
            }

            // property
            ret += &format!(
                "{} {};{}\n",
                full_type,
                safe_name,
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
            self.return_type.as_safe().replace_missing(),
            self.function_name
        );
        for i in 0..self.arg_count {
            let name = format!("arg{}", i.to_string());
            ret += &format!(
                "{} {}{}",
                &self.arg_types[0].as_safe().replace_missing(),
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
struct GetterSetter {
    full_type: String,
    safe_name: String,
    full_get_name: String,
    full_set_name: String,
    prop: ClassProperty,
}

impl GetterSetter {
    fn new(prop: &ClassProperty) -> GetterSetter {
        let pascal = true; // TODO: will this work with map as a function param?
        let full_type = format!(
            "{}{}{}",
            if !prop.container_type.is_empty() {
                format!("{}<", prop.container_type)
            } else {
                String::from("")
            },
            prop.property_type.as_safe().replace_missing(),
            if !prop.container_type.is_empty() {
                String::from(">")
            } else {
                String::from("")
            }
        )
        .replace(">>", "> >");

        let mut safe_name = prop.property_name.as_safe().replace(".", "_");
        if pascal {
            safe_name = capitalize_first(&safe_name);
        }

        safe_name = safe_name.replace("SName", "Name");

        let mut full_get_name = format!(
            "{}{}",
            if pascal {
                String::from("Get")
            } else {
                String::from("get_")
            },
            if full_type == "bool" {
                if pascal {
                    String::from("Is") + &safe_name
                } else {
                    String::from("is_") + &safe_name
                }
            } else {
                safe_name.clone()
            }
        );

        full_get_name = full_get_name.replace("IsIs", "");

        let full_set_name = full_get_name.replace("get", "set").replace("Get", "Set");

        GetterSetter {
            full_type,
            safe_name,
            full_get_name,
            full_set_name,
            prop: prop.clone(),
        }
    }

    fn serialize(&self) -> String {
        let mut ret = String::from("");

        // getter
        if !self.prop.property_note.is_empty()
            && self.prop.property_note.replace("m_", "") != self.prop.property_name.as_safe()
        {
            ret += &format!("\n     // Get {}", self.prop.property_note);
        }
        ret += &format!(
            "\n     auto {}() {{ return reinterpret_cast<{}*>(reinterpret_cast<uintptr_t>(this) + {}); }}\n",
            self.full_get_name, self.full_type, self.prop.offset,
        );

        if !self.prop.property_note.is_empty()
            && self.prop.property_note.replace("m_", "") != self.prop.property_name.as_safe()
        {
            ret += &format!("\n     // Set {}", self.prop.property_note);
        }
        // setter
        ret += &format!(
            "\n     void {}({} val) {{ *reinterpret_cast<{}*>(reinterpret_cast<uintptr_t>(this) + {}) = val; }}\n",
            self.full_set_name, self.full_type, self.full_type, self.prop.offset,
        );
        ret
    }
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
    getters_setters: Vec<GetterSetter>,
}

impl WizClass {
    fn new(node: Node) -> WizClass {
        let class_name = node.attribute("Name").unwrap().to_string();

        let mut properties: Vec<ClassProperty> = node
            .children()
            .filter(|child| child.tag_name().name() == "Property")
            .map(ClassProperty::new)
            .collect();

        let mut known_enum_names = HashSet::new();
        for prop in &mut properties {
            if let Some(enum_info) = &mut prop.enum_info {
                for i in 0..enum_info.values.len() {
                    let enum_val = &enum_info.values[i];
                    if known_enum_names.contains(&enum_val.name) {
                        enum_info.values[i].name += "_";
                        continue;
                    }
                    known_enum_names.insert(enum_val.name.clone());
                }
            }
        }

        properties.sort_by_key(|prop| prop.offset.parse::<i32>().unwrap_or(0));

        let getters_setters = properties.iter().map(GetterSetter::new).collect();

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
            getters_setters,
        }
    }

    fn is_empty(&self) -> bool {
        let empty =
            self.functions.is_empty() && self.sub_classes.is_empty() && self.properties.is_empty();
        return empty;
    }

    fn len(&self) -> usize {
        self.functions.len() + self.sub_classes.len() + self.properties.len()
    }

    fn serialize(&self, sub_class: bool, total_size: i64) -> String {
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
            /*if self.is_empty() {
                String::from("")
            } else {*/
            format!(
                "{}public:\n",
                if sub_class {
                    String::from("     ")
                } else {
                    String::from("")
                },
            ) //}
        );

        if total_size < 0 {
            class_def +=
                "\n     /*  error calculating total class size, summed parent sizes > this class size */      \n"
        } else {
            if total_size > 0 {
                class_def += &format!("     uint8_t padding[{}];\n", total_size.to_string());
            }
        }

        let mut sorted = self.sub_classes.clone();
        sorted.sort_by_key(|c| c.len());

        for sub_class in &sorted {
            class_def += &sub_class.serialize(true, 123); // TODO: do this properly
        }

        for property in &self.properties {
            if property.enum_info.is_some() {
                class_def += &format!(
                    "     {}{}",
                    if sub_class {
                        if !self.is_empty() {
                            String::from("     ")
                        } else {
                            String::from("")
                        }
                    } else {
                        String::from("")
                    },
                    property.clone().serialize()
                );
            }
        }

        for function in &self.functions {
            /*class_def += &format!(
                "     {}{}",
                if sub_class {
                    String::from("     ")
                } else {
                    String::from("")
                },
                function.clone().serialize()
            );*/
        }

        for getter_setter in &self.getters_setters {
            if getter_setter.prop.enum_info.is_none() {
                class_def += &getter_setter.serialize();
            }
        }

        // lua initializers:
        class_def += "\n     static void initialize_lua(lua_State* L) {\n";
        class_def += "          luabridge::getGlobalNamespace(L)\n";
        if let Some(base_class_name) = self.base_class_name.clone() {
            class_def += &format!(
                "               .deriveClass<{}, {}>(\"{}\")\n",
                self.class_name.as_safe(),
                base_class_name.as_safe(),
                self.class_name.as_safe(),
            );
        } else {
            class_def += &format!(
                "               .beginClass<{}>(\"{}\")\n",
                self.class_name.as_safe(),
                self.class_name.as_safe(),
            );
        }
        for getter_setter in &self.getters_setters {
            /*if getter_setter.prop.enum_info.is_none() {
                class_def += &format!(
                    "                    .addProperty(\"{}\", &{}::{}, &{}::{})\n",
                    getter_setter.safe_name,
                    self.class_name.as_safe(),
                    getter_setter.full_get_name,
                    self.class_name.as_safe(),
                    getter_setter.full_set_name
                );
            }*/
            if getter_setter.prop.enum_info.is_none() {
                class_def += &format!(
                    "                    .addFunction(\"{}\", &{}::{})\n",
                    getter_setter.full_get_name,
                    self.class_name.as_safe(),
                    getter_setter.full_get_name,
                );
                class_def += &format!(
                    "                    .addFunction(\"{}\", &{}::{})\n",
                    getter_setter.full_set_name,
                    self.class_name.as_safe(),
                    getter_setter.full_set_name,
                );
            }
        }
        class_def += "               .endClass();\n";
        class_def += "     }\n";

        class_def += &format!(
            "{}}};\n",
            if sub_class {
                String::from("     ")
            } else {
                String::from("")
            }
        );

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
                    (node.tag_name().name() == "Class"
                        && !node.attribute("Name").unwrap().contains("MadlibArgT")
                        && !node.attribute("Name").unwrap().contains("WeightedEntryT"))
                    .then(|| WizClass::new(node))
                })
                .collect::<Vec<_>>(),
        }
    }

    fn populate_subclasses_and_namespaces(&mut self) {
        let cloned = self.classes.clone();
        for i in (0..self.classes.len()).rev() {
            let class = &cloned[i];
            if let Some(mut top_level) = class.class_name.get_namespace() {
                top_level = top_level.replace("struct", "class");
                if let Some(parent_idx) = self.find_idx_by_name(top_level.clone()) {
                    self.classes[parent_idx].sub_classes.push(class.clone());
                    self.classes[i].is_subclass = true;
                } else {
                    self.classes[i].namespace = Some(top_level);
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

    fn find_idx_by_name(&self, name: String) -> Option<usize> {
        if let Some((index, _)) = self
            .classes
            .iter()
            .enumerate()
            .find(|(_, package)| package.class_name == name)
        {
            Some(index)
        } else {
            None
        }
    }

    fn find_maybe_namespace(&self, name: String) -> Option<WizClass> {
        // check before ::
        if let Some(namespace) = name.get_namespace() {
            if let Some(namespace_class) = self.find_by_name(namespace) {
                return Some(namespace_class);
            }
        }
        // check whole classname
        if let Some(first_look) = self.find_by_name(name.clone()) {
            return Some(first_look);
        }

        // check after namespace
        if let Some(named) = self.find_by_name(name.remove_namespace()) {
            return Some(named);
        }

        return None;
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

fn collect_deps(class: &WizClass, class_container: &ClassesContainer) -> Vec<Dependency> {
    if class.is_subclass {
        return vec![];
    }

    let mut dependencies = HashSet::new();

    if let Some(base_class_name) = &class.base_class_name {
        if let Some(base_class) = class_container.find_maybe_namespace(base_class_name.to_string())
        {
            dependencies.insert(base_class.class_name);
        }
    }

    for prop in &class.properties {
        if !prop.property_type.is_empty() {
            if let Some(prop_class) = class_container.find_maybe_namespace(
                prop.property_type
                    .to_string()
                    .replace("*", "")
                    .remove_sharedptr()
                    .remove_duplicate_class(),
            ) {
                dependencies.insert(prop_class.class_name);
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
        let subclass_deps = collect_deps(subclass, class_container);
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
    fn from(class: &WizClass, class_container: &ClassesContainer) -> Package {
        Package {
            name: class.class_name.clone(),
            dependencies: collect_deps(class, class_container),
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
    let mut f = File::open("./test/Bin/WizardClientDefs.xml")?;
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

    let valid_classes: Vec<WizClass> = classes
        .classes
        .iter()
        .cloned()
        .filter(|class| if class.is_subclass { false } else { true })
        .collect::<Vec<_>>();

    let mut packages: Vec<Package> = vec![];
    for class in valid_classes {
        packages.push(Package::from(&class, &classes));
    }

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
        "#ifndef MIDAS_COMMON_H
#define MIDAS_COMMON_H
#include <string>
#include <memory>
#include <cstdint>
#include <vector>
#include <list>
#include <locale>
#include <codecvt>
#include <array>

// lua includes

#include <lua.h>
#include <lualib.h>
#include <luacode.h>
#include <LuaBridge/LuaBridge.h>
#include <LuaBridge/Vector.h>

/* PROPERTYCLASS TYPE DEFINITIONS */
using gid = uint64_t;

using ObjectType = intptr_t;

using SerializedBuffer = intptr_t;

using Euler = intptr_t; // what??

using MISSING_TYPE = int; // missing from WizardClientDefs

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
    T value;
};

struct Matrix3x3 {
    std::array<float, 3> i;
    std::array<float, 3> j;
    std::array<float, 3> k;
};

struct SimpleFace {
    std::array<int, 3> vertexIndices; // Assuming triangular faces
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

struct Quaternion {
    double w;
    double x;
    double y;
    double z;
};

struct s24 {
    int data : 24;
};

struct u24 {
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
void init_common_types(lua_State *L)
{
    luabridge::getGlobalNamespace(L)
        .beginClass<Rect<int>>(\"Rect\")
        .addProperty(\"top\", &Rect<int>::top)
        .addProperty(\"left\", &Rect<int>::left)
        .addProperty(\"right\", &Rect<int>::right)
        .addProperty(\"bottom\", &Rect<int>::bottom)
        .endClass()
        .beginClass<Point<int>>(\"PointInt\")
        .addProperty(\"x\", &Point<int>::x)
        .addProperty(\"y\", &Point<int>::y)
        .endClass()
        .beginClass<Point<float>>(\"PointFloat\")
        .addProperty(\"x\", &Point<float>::x)
        .addProperty(\"y\", &Point<float>::y)
        .endClass()
        .beginClass<Size<int>>(\"Size\")
        .addProperty(\"value\", &Size<int>::value)
        .endClass()
        .beginClass<Matrix3x3>(\"Matrix3x3\")
        .addProperty(\"i\", &Matrix3x3::i)
        .addProperty(\"j\", &Matrix3x3::j)
        .addProperty(\"k\", &Matrix3x3::k)
        .endClass()
        .beginClass<SimpleFace>(\"SimpleFace\")
        .addProperty(\"i\", &SimpleFace::vertexIndices)
        .endClass()
        .beginClass<SimpleVert>(\"SimpleVert\")
        .addProperty(\"x\", &SimpleVert::x)
        .addProperty(\"y\", &SimpleVert::y)
        .addProperty(\"z\", &SimpleVert::z)
        .endClass()
        .beginClass<Vector3D>(\"Vector3D\")
        .addProperty(\"x\", &Vector3D::x)
        .addProperty(\"y\", &Vector3D::y)
        .addProperty(\"z\", &Vector3D::z)
        .endClass()
        .beginClass<Color>(\"Color\")
        .addProperty(\"b\", &Color::b)
        .addProperty(\"r\", &Color::r)
        .addProperty(\"g\", &Color::g)
        .addProperty(\"a\", &Color::a)
        .endClass()
        .beginClass<Quaternion>(\"Quaternion\")
        .addProperty(\"w\", &Quaternion::w)
        .addProperty(\"x\", &Quaternion::x)
        .addProperty(\"y\", &Quaternion::y)
        .addProperty(\"z\", &Quaternion::z)
        .endClass();
}\n",
    );

    ret += "#endif //MIDAS_COMMON_H";

    std::fs::create_dir("midas_types");

    let mut common_file = File::create("midas_types/common.h").unwrap();
    common_file.write_all(ret.as_bytes()).unwrap();

    let mut includes_text = String::from("");
    let mut serialized_classes = HashSet::new(); // Track serialized classes
    for package in graph {
        match package {
            Step::Resolved(package) => {
                // Check if the class has already been serialized
                if serialized_classes.contains(&package.name)
                    || package.name.contains("class MapInfoManager::MapInfo")
                {
                    continue; // Skip serialization
                }

                if let Some(found_class) = classes.find_by_name(package.name.clone()) {
                    let mut total_size = found_class.size.parse::<i64>().unwrap();
                    if found_class.base_class_name.is_some() {
                        let mut parent_name = found_class.clone().base_class_name.unwrap();
                        while let Some(parent_class) = classes.find_by_name(parent_name) {
                            total_size -= parent_class.size.parse::<i64>().unwrap();
                            if parent_class.base_class_name.is_some() {
                                parent_name = parent_class.base_class_name.unwrap();
                            } else {
                                break;
                            }
                        }
                    }

                    /*let mut real_name = if let Some(namespace) = &found_class.namespace {
                        found_class
                            .class_name
                            .as_safe()
                            .replace(&format!("{}::", namespace.as_safe()), "")
                    } else {
                        found_class.class_name.as_safe()
                    };*/
                    let mut real_name = found_class.class_name.as_safe().remove_namespace();

                    includes_text += &format!("#include \"{}.h\"\n", real_name);

                    if found_class.is_subclass {
                        real_name = real_name.remove_namespace();
                    }

                    let mut classes_file_text = String::from(format!(
                        "#ifndef MIDAS_{}_H
#define MIDAS_{}_H\n
#include \"common.h\"\n",
                        real_name, real_name
                    ));

                    for dep in package.dependencies.clone() {
                        let dep_name = dep.name.remove_namespace().as_safe();
                        if dep_name.contains("Point<")
                            || dep_name.contains("Size<")
                            || dep_name.contains("Rect<")
                            || dep_name.contains("Delegate<")
                        {
                            continue;
                        }
                        match dep_name.as_str() {
                            "std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >" => {},
                            "std::basic_string<wchar_t,struct std::char_traits<wchar_t>,class std::allocator<wchar_t> >" => {},
                            "std::basic_string<wchar_t,std::char_traits<wchar_t>,std::allocator<wchar_t> >" => {},
                            "float" => {},
                            "int" => {},
                            "std::string" => {},
                            "std::wstring" => {},
                            "unsigned int" => {},
                            "unsigned long" => {},
                            "void" => {},
                            "bool" => {},
                            "char" => {},
                            "Color" => {},
                            "gid" => {},
                            "uint64_t" => {},
                            "double" => {},
                            "Vector3D" => {},
                            "ObjectType" => {},
                            "CrownShopViews" => {},
                            "Item" => {},
                            "ProxyType" => {},

                            _ => classes_file_text += &format!("#include \"{}.h\"\n", dep_name)
                        }
                    }

                    classes_file_text += &format!(
                        "\n{}\n",
                        found_class
                            .serialize(false, total_size)
                            .replace(">>", "> >")
                    );

                    classes_file_text += &format!("#endif //MIDAS_{}_H", real_name);

                    let mut class_file =
                        File::create(&format!("midas_types/{}.h", real_name)).unwrap();
                    class_file.write_all(classes_file_text.as_bytes()).unwrap();

                    serialized_classes.insert(package.name.clone());
                }
            }
            Step::Unresolved(_) => {}
        }
    }

    //let mut class_file = File::create("midas_types/includes.h").unwrap();
    //class_file.write_all(includes_text.as_bytes()).unwrap();
}
