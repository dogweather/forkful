---
title:                "Arbeid med YAML"
date:                  2024-01-19
html_title:           "Arduino: Arbeid med YAML"
simple_title:         "Arbeid med YAML"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML er et dataformat for konfigurasjonsfiler. Programmerere bruker YAML for å enkelt lese og skrive datastrukturer, samt konfigurere applikasjoner.

## How to:
For å jobbe med YAML i Java, kan du bruke biblioteket `snakeyaml`. Nedenfor er et eksempel på hvordan du kan lese en YAML-fil.

```java
import org.yaml.snakeyaml.Yaml;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.util.Map;

public class YamlDemo {
    public static void main(String[] args) throws FileNotFoundException {
        Yaml yaml = new Yaml();
        FileInputStream inputStream = new FileInputStream("config.yaml");
        Map<String, Object> data = yaml.load(inputStream);
        System.out.println(data);
    }
}
```

Assuming `config.yaml` er:

```yaml
database:
  host: localhost
  port: 3306

logging:
  level: INFO
  format: compact
```

Sample output:

```plaintext
{database={host=localhost, port=3306}, logging={level=INFO, format=compact}}
```

## Deep Dive
YAML, som står for "YAML Ain't Markup Language", ble introdusert på begynnelsen av 2000-tallet og er et menneskelesbart data-serialiseringsformat. Alternativer inkluderer JSON og XML, men YAML er ofte valgt for sin leslighet. Ved implementering i Java bruker de fleste en ekstern pakke som `snakeyaml` fordi det ikke er et innebygd YAML-bibliotek i Java.

## See Also
- [Official YAML website](https://yaml.org)
