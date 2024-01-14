---
title:                "Java recipe: Working with yaml"
simple_title:         "Working with yaml"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/working-with-yaml.md"
---

{{< edit_this_page >}}

##Why
YAML, or "YAML Ain't Markup Language", is a human-readable data serialization language that is commonly used for configuration files. It offers a simple syntax that allows for easily defining and organizing data, making it a popular choice for developers.

##How To
Using YAML in Java is straightforward and requires only a few steps. First, you will need to add a YAML library to your project's dependencies, such as SnakeYAML or Jackson YAML. Then, you can start working with YAML files by using the classes and methods provided by the library.

Here is an example of how to read a YAML file using SnakeYAML:

```Java
import org.yaml.snakeyaml.Yaml;
import java.io.InputStream;

public class YamlExample {
    public static void main(String[] args) {
        //get input stream of YAML file
        InputStream input = YamlExample.class.getResourceAsStream("config.yaml");
        
        //create YAML object
        Yaml yaml = new Yaml();
        
        //parse the YAML input into a Map
        Map<String, String> config = yaml.load(input);
        
        //print values from the map
        System.out.println(config.get("database.host"));
        System.out.println(config.get("database.port"));
        System.out.println(config.get("database.username"));
        System.out.println(config.get("database.password"));
    }
}
```

Sample output:
```
localhost
5432
admin
password123
```

##Deep Dive
Working with YAML in Java offers many predefined classes and methods that make it convenient for developers to manipulate and access data. For example, the `Yaml` class in the SnakeYAML library provides methods like `load()` and `dump()` that allow for easy conversion between a YAML string and a Java object.

In addition, YAML supports various data structures such as lists, maps, and scalars, which can all be represented in Java using the corresponding data types. It also offers features like anchors and aliases, which can be useful for referencing data within the YAML file.

It's worth noting that while YAML is typically used for configuration files, it can also be used for other purposes such as storing and exchanging data between different systems.

##See Also
- [SnakeYAML website](https://bitbucket.org/asomov/snakeyaml)
- [Jackson YAML website](https://github.com/FasterXML/jackson-dataformats-text/tree/master/yaml)
- [Official YAML specification](https://yaml.org/spec/)