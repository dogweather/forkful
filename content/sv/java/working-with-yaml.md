---
title:                "Arbete med YAML"
date:                  2024-01-19
html_title:           "Arduino: Arbete med YAML"
simple_title:         "Arbete med YAML"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/working-with-yaml.md"
---

{{< edit_this_page >}}

## Vad & Varför?
YAML handlar om att hantera dataformat som används för konfigurationsfiler och dataserier. Programmerare använder det för dess läsbarhet och enkelhet, vilket underlättar konfiguration och datautbyte.

## Hur gör man:
För att jobba med YAML i Java, använd biblioteket `SnakeYAML`. Så här ser en grundläggande användning ut:

```java
import org.yaml.snakeyaml.Yaml;
import java.util.Map;

public class YamlExample {
    public static void main(String[] args) {
        String yamlStr = "name: Ylva\nage: 35\nlanguage: Java";
        Yaml yaml = new Yaml();
        Map<String, Object> data = yaml.load(yamlStr);

        System.out.println(data);
    }
}
```

Kör koden, och du får:
```
{name=Ylva, age=35, language=Java}
```

## Djupdykning:
YAML började användas i början av 2000-talet som ett enklare alternativ till XML. YAML står för "YAML Ain't Markup Language" och är utformat för att vara mänskligt läsligt. När XML kan vara för krångligt och JSON saknar kommentarsmöjlighet, är YAML ett bra mellanalternativ. Snabbhet och minnestillgång kan vara sämre jämfört med JSON, så använd fallet styr valet.

## Se även:
- YAML officiell hemsida: https://yaml.org
- SnakeYAML GitHub-sida: https://github.com/asomov/snakeyaml
- En jämförelse mellan JSON och YAML: https://json2yaml.com/
