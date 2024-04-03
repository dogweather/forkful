---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:38.928456-07:00
description: "Kuinka: Javassa voit ty\xF6skennell\xE4 YAML-tiedostojen kanssa k\xE4\
  ytt\xE4m\xE4ll\xE4 kolmannen osapuolen kirjastoja, koska Java Standard Edition ei\
  \ sis\xE4ll\xE4\u2026"
lastmod: '2024-03-13T22:44:56.465981-06:00'
model: gpt-4-0125-preview
summary: "Javassa voit ty\xF6skennell\xE4 YAML-tiedostojen kanssa k\xE4ytt\xE4m\xE4\
  ll\xE4 kolmannen osapuolen kirjastoja, koska Java Standard Edition ei sis\xE4ll\xE4\
  \ sis\xE4\xE4nrakennettua tukea YAML:lle."
title: "Ty\xF6skentely YAML:n kanssa"
weight: 41
---

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
