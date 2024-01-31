---
title:                "Werken met YAML"
date:                  2024-01-28T22:12:03.465519-07:00
model:                 gpt-4-0125-preview
simple_title:         "Werken met YAML"

category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/java/working-with-yaml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

YAML, "YAML Ain't Markup Language," is een gegevensserialisatietaal. Programmeurs gebruiken het vanwege de leesbaarheid en eenvoud, met name voor configuratiebestanden, initiële gegevensdumps of als een communicatieformaat tussen verschillende systemen.

## Hoe te:

Om YAML in Java te gebruiken, laten we `snakeyaml` gebruiken, een populaire bibliotheek.

Voeg eerst de afhankelijkheid toe aan je `pom.xml`:

```xml
<dependency>
    <groupId>org.yaml</groupId>
    <artifactId>snakeyaml</artifactId>
    <version>1.29</version>
</dependency>
```

Nu, een YAML-bestand lezen:

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

Stel dat `config.yaml` er zo uitziet:

```yaml
version: '1.0'
services:
  webapp:
    build: .
    ports:
      - "5000:5000"
```

De uitvoer zal een `Map`-representatie van uw YAML zijn:

```
{version=1.0, services={webapp={build=., ports=[5000:5000]}}}
```

Nu, laten we YAML schrijven:

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

Controleer `output.yaml` om de nieuwe YAML-inhoud te zien:

```yaml
name: myapp
version: '2.0'
```

## Dieper duiken

YAML kwam begin jaren 2000 op de markt als een alternatief voor XML voor eenvoudigere gegevensstructurering. Terwijl de opkomst van JSON het voor API-communicatie overschaduwde, blijft YAML’s gebruiksvriendelijkheid populair voor configuraties. Dezelfde gegevens, maar JSON en TOML zijn alternatieven voor YAML, afhankelijk van gebruikssituaties. Een YAML waarschuwing: tabbladen zijn niet toegestaan voor inspringing; alleen spaties.

## Zie ook

Verken verder met deze bronnen:

- Officiële YAML Specificatie: https://yaml.org/spec/1.2.2/
- snakeyaml GitHub Repo: https://github.com/asomov/snakeyaml
- YAML vs JSON: https://phoenixnap.com/kb/yaml-vs-json
- YAML Lint, om uw YAML-bestanden te valideren: http://www.yamllint.com/
