---
title:                "Yaml-työskentely"
html_title:           "Java: Yaml-työskentely"
simple_title:         "Yaml-työskentely"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/working-with-yaml.md"
---

{{< edit_this_page >}}

YAML ja sen käyttäminen Java-ohjelmoinnissa

## Miksi

YAML (Yet Another Markup Language) on kevyt ja helppolukuinen tiedostomuoto, jota käytetään usein tiedostojen tallentamiseen ja siirtämiseen erilaisten ohjelmistojen välillä. Se on erityisen hyödyllinen Java-ohjelmoinnissa, koska sillä voidaan tallentaa tietoja rakenteellisessa muodossa, mikä tekee siitä kätevän käytettävänä olevan ohjelmakoodin tallentamiseen ja lataamiseen. 

## Miten

YAML-muotoa käytetään Java-ohjelmoinnissa yleensä YAML-kirjastojen avulla. Alla on esimerkki siitä, miten voit käyttää SnakeYAML-kirjastoa tallentaaksesi ja ladataksesi tietoja YAML-muotoisesta tiedostosta.

```Java
import java.io.FileWriter;
import java.io.IOException;
import org.yaml.snakeyaml.Yaml;

public class YamlExample {

    public static void main(String[] args) {
        
        // Luodaan YAML-kirjoittaja
        Yaml yaml = new Yaml();
        
        // Luodaan tietorakenne
        Map<String,String> tulot = new HashMap<>();
        tulot.put("2019", "50000€");
        tulot.put("2020", "60000€");
        
        // Tallennetaan tiedot YAML-muotoiseen tiedostoon
        try {
            FileWriter writer = new FileWriter("tulot.yml");
            yaml.dump(tulot, writer);
        } catch (IOException e) {
            e.printStackTrace();
        }
        
        // Luetaan tiedot YAML-muotoisesta tiedostosta
        try {
            Map<String,String> uudetTulot = yaml.load(new FileReader("tulot.yml"));
            System.out.println("Luettiin tulot:");
            for (String vuosi : uudetTulot.keySet()) {
                System.out.println("- " + vuosi + ": " + uudetTulot.get(vuosi));
            }
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
        
    }
}
```

Tämä koodiesimerkki luo yksinkertaisen YAML-tiedoston, tallentaa siihen tietoja ja lukee sitten tiedot takaisin. Output on seuraava:

```
Luettiin tulot:
- 2019: 50000€
- 2020: 60000€
```

## Syventävä tarkastelu

YAML ei ole vain kevyt ja helppolukuinen, vaan se on myös erittäin joustava tiedostomuoto. Se tukee useita erilaisia arvotyyppejä, kuten merkkijonoja, numeroita, listoja ja karttoja. Lisäksi YAML-muoto mahdollistaa myös sisäkkäisten tietorakenteiden käytön, mikä tekee siitä kätevän tietojen tallentamiseen monimutkaisempia tietorakenteita varten.

YAML on myös helposti luettavissa ja muokattavissa ihmisten toimesta, mikä tekee siitä muutosten tekemisen ja virheiden korjaamisen helpoksi ohjelmoidessa.

## Katso myös

- SnakeYAML-kirjasto: https://bitbucket.org/asomov/snakeyaml
- YAML-spesifikaatio: https://yaml.org/spec/
- Esimerkki YAML-tiedostosta: http://yaml.org/start.html