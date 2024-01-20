---
title:                "Working with yaml"
html_title:           "Arduino recipe: Working with yaml"
simple_title:         "Working with yaml"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?

YAML, "YAML Ain't Markup Language," is a data serialization language. Programmers use it due to its readability and simplicity, particularly for config files, initial data dumps, or as a communication format between different systems.

## How to:

To handle YAML in Java, let's use `snakeyaml`, a popular lib.

First, add the dependency to your `pom.xml`:

```xml
<dependency>
    <groupId>org.yaml</groupId>
    <artifactId>snakeyaml</artifactId>
    <version>1.29</version>
</dependency>
```

Now, read a YAML file:

```java
import org.yaml.snakeyaml.Yaml;
import java.io.InputStream;
import java.util.Map;

public class YamlReader {
    public static void main(String[] args) {
        Yaml yaml = new Yaml();
        try (InputStream in = YamlReader.class
            .getClassLoader()
            .getResourceAsStream("config.yaml")) {
            
            Map<String, Object> data = yaml.load(in);
            System.out.println(data);
            
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

Suppose `config.yaml` looks like this:

```yaml
version: '1.0'
services:
  webapp:
    build: .
    ports:
      - "5000:5000"
```

The output will be a `Map` representation of your YAML:

```
{version=1.0, services={webapp={build=., ports=[5000:5000]}}}
```

Now, let's write YAML:

```java
import org.yaml.snakeyaml.Yaml;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

public class YamlWriter {
    public static void main(String[] args) {
        Yaml yaml = new Yaml();
        Map<String, Object> data = new HashMap<>();
        
        data.put("name", "myapp");
        data.put("version", "2.0");
        
        try (FileWriter writer = new FileWriter("output.yaml")) {
            yaml.dump(data, writer);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

Check `output.yaml` to see the new YAML content:

```yaml
name: myapp
version: '2.0'
```

## Deep Dive

YAML hit the scene in the early 2000s as an alternative to XML for simpler data structuring. While JSON's rise overshadowed it for API communication, YAML’s human-friendliness keeps it popular for configs. Same data, but JSON and TOML are alternatives to YAML, depending on use cases. One YAML caveat: tabs aren't allowed for indentation; spaces only.

## See Also

Explore further with these resources:

- Official YAML Spec: https://yaml.org/spec/1.2.2/
- snakeyaml GitHub Repo: https://github.com/asomov/snakeyaml
- YAML vs JSON: https://phoenixnap.com/kb/yaml-vs-json
- YAML Lint, to validate your YAML files: http://www.yamllint.com/