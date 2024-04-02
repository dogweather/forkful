---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:36.393245-07:00
description: "YAML, kurz f\xFCr \"YAML Ain't Markup Language\", ist ein f\xFCr Menschen\
  \ lesbarer Daten-Serialisierungsstandard, den Programmierer f\xFCr Konfigurationsdateien,\
  \ das\u2026"
lastmod: '2024-03-13T22:44:53.782638-06:00'
model: gpt-4-0125-preview
summary: "YAML, kurz f\xFCr \"YAML Ain't Markup Language\", ist ein f\xFCr Menschen\
  \ lesbarer Daten-Serialisierungsstandard, den Programmierer f\xFCr Konfigurationsdateien,\
  \ das\u2026"
title: Arbeiten mit YAML
weight: 41
---

## Was & Warum?
YAML, kurz für "YAML Ain't Markup Language", ist ein für Menschen lesbarer Daten-Serialisierungsstandard, den Programmierer für Konfigurationsdateien, das Dumpen von Daten und den Datenaustausch zwischen Sprachen verwenden. Es ist aufgrund seiner Lesbarkeit und Benutzerfreundlichkeit beliebt, was es zu einer gängigen Wahl für die Konfiguration von Anwendungen und Diensten macht.

## Wie geht das:
In Java können Sie mit YAML-Dateien arbeiten, indem Sie Drittanbieter-Bibliotheken verwenden, da die Java Standard Edition keine integrierte Unterstützung für YAML bietet. Eine beliebte Bibliothek ist SnakeYAML, die das Parsen und Generieren von YAML-Daten erleichtert.

### SnakeYAML einrichten
Fügen Sie zunächst SnakeYAML Ihrem Projekt hinzu. Wenn Sie Maven verwenden, fügen Sie die folgende Abhängigkeit zu Ihrer `pom.xml` hinzu:

```xml
<dependency>
    <groupId>org.yaml</groupId>
    <artifactId>snakeyaml</artifactId>
    <version>1.30</version>
</dependency>
```

### YAML lesen
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
Angenommen, `config.yml` sieht so aus:
```yaml
name: Beispiel
version: 1.0
features:
  - login
  - signup
```
Die Ausgabe wird sein:
```
{name=Beispiel, version=1.0, features=[login, signup]}
```

### YAML schreiben
Um ein YAML aus Java-Objekten zu generieren, verwenden Sie die `dump`-Methode, die von SnakeYAML bereitgestellt wird:
```java
import org.yaml.snakeyaml.Yaml;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.Map;

public class WriteYamlExample {
    public static void main(String[] args) {
        Map<String, Object> data = new LinkedHashMap<>();
        data.put("name", "Beispiel");
        data.put("version", 1.0);
        data.put("features", Arrays.asList("login", "signup"));

        Yaml yaml = new Yaml();
        String output = yaml.dump(data);
        System.out.println(output);
    }
}
```
Dies wird den folgenden YAML-Inhalt generieren und ausgeben:
```yaml
name: Beispiel
version: 1.0
features:
- login
- signup
```
Indem sie SnakeYAML nutzen, können Java-Entwickler das Parsen und Generieren von YAML leicht in ihre Anwendungen integrieren, wobei sie von der Lesbarkeit und Einfachheit von YAML für Konfigurations- und Datenaustauschzwecke profitieren.
