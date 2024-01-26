---
title:                "Lavorare con YAML"
html_title:           "Bash: Lavorare con YAML"
simple_title:         "Lavorare con YAML"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
Lavorare con YAML significa manipolare e gestire dati in un formato leggibile dall'uomo per la configurazione di applicazioni. I programmatori lo usano per la sua semplicità e facilità di comprensione rispetto a formati come JSON o XML.

## How to:
Per lavorare con YAML in Java, useremo la libreria SnakeYAML. Prima, assicurati di aggiungere la dipendenza al tuo `pom.xml` se stai usando Maven:

```xml
<dependency>
    <groupId>org.yaml</groupId>
    <artifactId>snakeyaml</artifactId>
    <version>1.29</version>
</dependency>
```

Esempio di codice:

```java
import org.yaml.snakeyaml.Yaml;
import java.io.InputStream;
import java.util.Map;

public class YamlExample {
    public static void main(String[] args) {
        Yaml yaml = new Yaml();
        try (InputStream inputStream = YamlExample.class
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

Supponendo che `config.yaml` sia:

```yaml
database:
  host: localhost
  port: 3306
```

Output di esempio:

```plaintext
{database={host=localhost, port=3306}}
```

## Deep Dive:
YAML, che sta per "YAML Ain't Markup Language", è nato nei primi anni 2000 come alternativa meno ingombrante e più leggibile rispetto a XML. Sebbene JSON sia un altro popolare formato di scambio dati, YAML è spesso preferito per la configurazione grazie alla sua enfasi sulla chiarezza. Tra le sue features ci sono i commenti, supporto per le relazioni complesse e tipi di dati scalabili. Le alternative a SnakeYAML includono Jackson e la libreria yamlbeans.

## See Also:
- [SnakeYAML Wiki](https://bitbucket.org/asomov/snakeyaml/wiki/Documentation)
- [YAML Official Site](https://yaml.org)
- [Jackson YAML Processor](https://github.com/FasterXML/jackson-dataformats-text/tree/master/yaml)
