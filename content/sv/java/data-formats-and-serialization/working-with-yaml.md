---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:37.033311-07:00
description: "Hur man g\xF6r: I Java kan du arbeta med YAML-filer med hj\xE4lp av\
  \ tredjepartsbibliotek eftersom Java Standard Edition inte inkluderar inbyggt st\xF6\
  d f\xF6r YAML.\u2026"
lastmod: '2024-03-13T22:44:37.807379-06:00'
model: gpt-4-0125-preview
summary: "I Java kan du arbeta med YAML-filer med hj\xE4lp av tredjepartsbibliotek\
  \ eftersom Java Standard Edition inte inkluderar inbyggt st\xF6d f\xF6r YAML."
title: Att Arbeta med YAML
weight: 41
---

## Hur man gör:
I Java kan du arbeta med YAML-filer med hjälp av tredjepartsbibliotek eftersom Java Standard Edition inte inkluderar inbyggt stöd för YAML. Ett populärt bibliotek är SnakeYAML, som tillåter parsing och generering av YAML-data på ett enkelt sätt.

### Sätta upp SnakeYAML
Först, inkludera SnakeYAML i ditt projekt. Om du använder Maven, lägg till beroendet nedan i din `pom.xml`:

```xml
<dependency>
    <groupId>org.yaml</groupId>
    <artifactId>snakeyaml</artifactId>
    <version>1.30</version>
</dependency>
```

### Läsa YAML
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
Antag att `config.yml` ser ut så här:
```yaml
name: Example
version: 1.0
features:
  - login
  - signup
```
Utmatningen blir:
```
{name=Example, version=1.0, features=[login, signup]}
```

### Skriva YAML
För att generera en YAML från Java-objekt, använd metoden `dump` som tillhandahålls av SnakeYAML:
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
Detta kommer att generera och skriva ut följande YAML-innehåll:
```yaml
name: Example
version: 1.0
features:
- login
- signup
```
Genom att utnyttja SnakeYAML kan Java-utvecklare enkelt integrera YAML-parsing och generering i sina applikationer, och dra nytta av YAML:s läsbarhet och enkelhet för konfigurations- och datautbytesändamål.
