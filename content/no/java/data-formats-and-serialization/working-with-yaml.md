---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:39.111699-07:00
description: "YAML, forkortet for \"YAML Ain't Markup Language\", er en menneskelesbar\
  \ standard for data-serialisering som programmerere bruker for konfigurasjonsfiler,\u2026"
lastmod: '2024-02-25T18:49:38.866643-07:00'
model: gpt-4-0125-preview
summary: "YAML, forkortet for \"YAML Ain't Markup Language\", er en menneskelesbar\
  \ standard for data-serialisering som programmerere bruker for konfigurasjonsfiler,\u2026"
title: Arbeider med YAML
---

{{< edit_this_page >}}

## Hva & Hvorfor?
YAML, forkortet for "YAML Ain't Markup Language", er en menneskelesbar standard for data-serialisering som programmerere bruker for konfigurasjonsfiler, data dumping, og datatransmisjon mellom språk. Det er populært på grunn av sin lesbarhet og enkelhet i bruk, noe som gjør det til et vanlig valg for konfigurering av applikasjoner og tjenester.

## Hvordan:
I Java kan du jobbe med YAML-filer ved å bruke tredjeparts biblioteker siden Java Standard Edition ikke inkluderer innebygd støtte for YAML. Et populært bibliotek er SnakeYAML, som tillater parsing og generering av YAML-data enkelt.

### Sette opp SnakeYAML
Først, inkluder SnakeYAML i prosjektet ditt. Hvis du bruker Maven, legg til følgende avhengighet i din `pom.xml`:

```xml
<dependency>
    <groupId>org.yaml</groupId>
    <artifactId>snakeyaml</artifactId>
    <version>1.30</version>
</dependency>
```

### Lese YAML
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
Forutsatt at `config.yml` ser slik ut:
```yaml
name: Example
version: 1.0
features:
  - login
  - signup
```
Utskriften vil være:
```
{name=Example, version=1.0, features=[login, signup]}
```

### Skrive YAML
For å generere en YAML fra Java-objekter, bruk `dump`-metoden som SnakeYAML tilbyr:
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
Dette vil generere og skrive ut følgende YAML-innhold:
```yaml
name: Example
version: 1.0
features:
- login
- signup
```
Ved å benytte SnakeYAML, kan Java-utviklere enkelt integrere YAML-parsing og generering i sine applikasjoner, og dra nytte av YAMLs lesbarhet og enkelhet for konfigurering og datautvekslingsformål.
