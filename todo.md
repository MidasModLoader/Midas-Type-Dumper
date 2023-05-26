TODO:
[] Combine namespaces
[] Make getters and setters for each property to avoid holes in defs
[] Validate if parent class is real by checking in all classes, i.e. Search::ResultItem <--- `Search` is a namespace, so during dep graph we get Search as top class, but it won't exist in lookup
[] Handle templated properties such as struct MadlibArgT<std::string>