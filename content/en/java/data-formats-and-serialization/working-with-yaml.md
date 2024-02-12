---
title:                "Working with YAML"
aliases: - /en/java/working-with-yaml.md
date:                  2024-02-03T19:03:12.830996-07:00
model:                 gpt-4-0125-preview
simple_title:         "Working with YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML, short for "YAML Ain't Markup Language," is a human-readable data serialization standard that programmers use for configuration files, data dumping, and data transmission between languages. It's popular due to its readability and ease of use, making it a common choice for configuring applications and services.

## How to:
In Java, you can work with YAML files using third-party libraries since the Java Standard Edition does not include built-in support for YAML. One popular library is SnakeYAML, which allows for parsing and generating YAML data easily.

### Setting up SnakeYAML
First, include SnakeYAML in your project. If you're using Maven, add the following dependency to your `pom.xml`:

```xml
<dependency>
    <groupId>org.yaml</groupId>
    <artifactId>snakeyaml</artifactId>
    <version>1.30</version>
</dependency>
```

### Reading YAML
```java
import org.yaml.snakeyaml.Yaml;
import java.io.InputStream;
import java.util.Map;

public class ReadYamlExample {
    public static void main(String[] args) {
        Yaml yaml = new Yaml();
        try (InputStream inputStream = ReadYamlExample.class
                .getClassLoader()
                .getResourceAsStream("config.yml")) {
            Map<String, Object> data = yaml.load(inputStream);
            System.out.println(data);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```
Assuming `config.yml` looks like this:
```yaml
name: Example
version: 1.0
features:
  - login
  - signup
```
The output will be:
```
{name=Example, version=1.0, features=[login, signup]}
```

### Writing YAML
To generate a YAML from Java objects, use the `dump` method provided by SnakeYAML:
```java
import org.yaml.snakeyaml.Yaml;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.Map;

public class WriteYamlExample {
    public static void main(String[] args) {
        Map<String, Object> data = new LinkedHashMap<>();
        data.put("name", "Example");
        data.put("version", 1.0);
        data.put("features", Arrays.asList("login", "signup"));

        Yaml yaml = new Yaml();
        String output = yaml.dump(data);
        System.out.println(output);
    }
}
```
This will generate and print the following YAML content:
```yaml
name: Example
version: 1.0
features:
- login
- signup
```
By leveraging SnakeYAML, Java developers can easily integrate YAML parsing and generation into their applications, benefiting from YAML's readability and simplicity for configuration and data exchange purposes.
