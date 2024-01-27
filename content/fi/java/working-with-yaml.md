---
title:                "YAML-tiedostojen käsittely"
date:                  2024-01-19
html_title:           "Arduino: YAML-tiedostojen käsittely"
simple_title:         "YAML-tiedostojen käsittely"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
YAML on ihmisen luettava data-sarjoitustapa, jota käytetään konfiguraatiotiedostoissa. Ohjelmoijat käyttävät sitä, koska se on selkeämpi kuin monet muut formaatit, ja sitä tukevat useat ohjelmointikielet.

## How to: (Kuinka tehdä:)
Java-koodissa YAML-tiedostojen käsittelyyn tarvitaan kirjasto, kuten `SnakeYAML`. Asenna se Maven-projektiisi lisäämällä riippuvuus pom.xml-tiedostoon:

```xml
<dependency>
    <groupId>org.yaml</groupId>
    <artifactId>snakeyaml</artifactId>
    <version>1.28</version>
</dependency>
```

Lukeaksesi YAML-tiedoston:

```java
import org.yaml.snakeyaml.Yaml;
import org.yaml.snakeyaml.constructor.Constructor;
import java.io.InputStream;
import java.util.Map;

public class YamlExample {
    public static void main(String[] args) {
        Yaml yaml = new Yaml(new Constructor(Map.class));
        InputStream inputStream = YamlExample.class
            .getClassLoader()
            .getResourceAsStream("config.yaml");
        Map<String, Object> data = yaml.load(inputStream);
        System.out.println(data);
    }
}
```

Tuloste:

```plaintext
{database={connection=postgresql://localhost/test, user=admin, password=secret}}
```

Kirjoittaaksesi YAML-tiedostoon:

```java
import org.yaml.snakeyaml.Yaml;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

public class YamlWriterExample {
    public static void main(String[] args) {
        Map<String, Object> data = new HashMap<>();
        data.put("database", new HashMap<String, String>() {{
            put("connection", "postgresql://localhost/test");
            put("user", "admin");
            put("password", "secret");
        }});

        Yaml yaml = new Yaml();
        try (FileWriter writer = new FileWriter("output.yaml")) {
            yaml.dump(data, writer);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

Tämä koodi luo `output.yaml` tiedoston sisältäen YAML-formaadissa olevat tiedot.

## Deep Dive (Syvä sukellus)

Historiallisesti YAML kehitettiin, koska tarvittiin helpommin luettava formaatti kuin XML. JSON:n kaltainen, mutta paremmin ihmisten luettavissa. Nimi oli aluksi lyhenne sanoista "Yet Another Markup Language", mutta nykyään se merkitsee "YAML Ain't Markup Language", korostaen sen datakeskeisyyttä.

Vaihtoehtoja YAML:lle ovat esimerkiksi JSON ja XML. Nämä formaatit ovat myös laajasti tuettuja, mutta niillä on omat käyttötarkoituksensa. YAML on usein parempi valinta, kun tarvitset selkeää konfiguraatiota tai kun tiedot ovat syvästi hierarkisia.

SnakeYAML on puhtaasti Java-kirjasto YAML:n parsimiseen ja tulostamiseen. Se noudattaa YAML 1.1 -standardia ja tarjoaa kyvyn lukea ja kirjoittaa kaikenlaista YAML-muotoista dataa. Käyttäessäsi SnakeYAML-kirjastoa, vastaat itse resurssien, kuten InputStreamin, hallinnasta.

## See Also (Lisätietoa)

- YAML-spesifikaatio: https://yaml.org/spec/1.2.2/
- SnakeYAML GitHub-sivu: https://github.com/asomov/snakeyaml
- Vertailu JSON:n ja YAML:n välillä: https://phoenixnap.com/kb/json-vs-yaml
- XML:n perusteet: https://www.w3schools.com/xml/xml_whatis.asp
