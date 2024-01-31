---
title:                "Arbeiten mit YAML"
date:                  2024-01-19
simple_title:         "Arbeiten mit YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/working-with-yaml.md"
---

{{< edit_this_page >}}

## Was & Warum?
YAML steht für "YAML Ain't Markup Language" und ist ein Format zum Speichern und Übertragen von Daten, das für Menschen einfach zu lesen ist. Java-Programmierer nutzen YAML häufig für Konfigurationsdateien, weil es übersichtlicher als XML oder JSON ist.

## How to:
Um YAML in Java zu nutzen, brauchst du eine Bibliothek wie `snakeyaml`. Hier ein Beispiel, wie du eine YAML-Datei einliest:

```java
import org.yaml.snakeyaml.Yaml;
import java.io.InputStream;
import java.util.Map;

public class YamlBeispiel {
    public static void main(String[] args) {
        Yaml yaml = new Yaml();
        try (InputStream inputStream = YamlBeispiel.class
                .getClassLoader()
                .getResourceAsStream("config.yaml")) {
            Map<String, Object> data = yaml.load(inputStream);
            System.out.println(data);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

Stelle sicher, dass `config.yaml` im classpath liegt. Die Ausgabe könnte so aussehen:

```java
{database=localhost, port=3306, username=admin, password=secret}
```

## Deep Dive:
YAML trat Anfang der 2000er Jahre auf. Es wurde als eine einfachere Alternative zu XML entworfen und wird bevorzugt, wenn die Lesbarkeit entscheidend ist. Neben `snakeyaml` gibt es Alternativen wie `jackson-dataformat-yaml`. Während `snakeyaml` hauptsächlich für das Lesen und Schreiben von YAML-Strukturen steht, bietet `jackson` umfangreiche Möglichkeiten wie Datenbindung und Schema-Validierung.

## See Also:
- Offizielle YAML-Website: [yaml.org](https://yaml.org)
- SnakeYAML Engine: [bitbucket.org/asomov/snakeyaml-engine](https://bitbucket.org/asomov/snakeyaml-engine)
- Jackson YAML-Modul: [github.com/FasterXML/jackson-dataformat-yaml](https://github.com/FasterXML/jackson-dataformat-yaml)
