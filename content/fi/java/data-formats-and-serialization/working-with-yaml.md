---
title:                "Työskentely YAML:n kanssa"
aliases:
- fi/java/working-with-yaml.md
date:                  2024-02-03T19:25:38.928456-07:00
model:                 gpt-4-0125-preview
simple_title:         "Työskentely YAML:n kanssa"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?
YAML, lyhenne sanoista "YAML Ain't Markup Language", on ihmisen luettavissa oleva datan serialisointistandardi, jota ohjelmoijat käyttävät konfiguraatiotiedostoihin, datan dumpaamiseen ja datan siirtoon kielten välillä. Se on suosittu sen luettavuuden ja käyttöhelppouden vuoksi, mikä tekee siitä yleisen valinnan sovellusten ja palveluiden konfigurointiin.

## Kuinka:
Javassa voit työskennellä YAML-tiedostojen kanssa käyttämällä kolmannen osapuolen kirjastoja, koska Java Standard Edition ei sisällä sisäänrakennettua tukea YAML:lle. Yksi suosittu kirjasto on SnakeYAML, joka mahdollistaa YAML-datan jäsentämisen ja generoinnin helposti.

### SnakeYAML:n asettaminen
Lisää ensin SnakeYAML projektiisi. Jos käytät Mavenia, lisää seuraava riippuvuus `pom.xml`-tiedostoosi:

```xml
<dependency>
    <groupId>org.yaml</groupId>
    <artifactId>snakeyaml</artifactId>
    <version>1.30</version>
</dependency>
```

### YAML:n lukeminen
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
Olettaen, että `config.yml` näyttää tältä:
```yaml
name: Example
version: 1.0
features:
  - login
  - signup
```
Tuloste on:
```
{name=Example, version=1.0, features=[login, signup]}
```

### YAML:n kirjoittaminen
Javan objekteista YAML:n generointiin, käytä SnakeYAML:n tarjoamaa `dump`-metodia:
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
Tämä generoi ja tulostaa seuraavan YAML-sisällön:
```yaml
name: Example
version: 1.0
features:
- login
- signup
```
Hyödyntämällä SnakeYAML:ää, Java-kehittäjät voivat helposti integroida YAML:n jäsentämisen ja generoinnin sovelluksiinsa, hyötyen YAML:n luettavuudesta ja yksinkertaisuudesta konfiguraation ja datan vaihdon tarkoituksiin.
