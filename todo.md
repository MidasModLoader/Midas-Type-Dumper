TODO:
[Xish] Combine namespaces
[X] Make getters and setters for each property to avoid holes in defs
[X] Validate if parent class is real by checking in all classes, i.e. Search::ResultItem <--- `Search` is a namespace, so during dep graph we get Search as top class, but it won't exist in lookup
[X] Handle templated properties such as struct MadlibArgT<std::string>
