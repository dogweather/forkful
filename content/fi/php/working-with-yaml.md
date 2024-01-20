---
title:                "Työskentely yaml:n kanssa"
html_title:           "PHP: Työskentely yaml:n kanssa"
simple_title:         "Työskentely yaml:n kanssa"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/working-with-yaml.md"
---

{{< edit_this_page >}}

## Mitä se on & miksi se on tärkeää?
YAML on yksi monista tiedostomuodoista, joita ohjelmoijat voivat käyttää tallentamaan ja lukemaan tiedon keräämistä varten. Se on erityisen suosittu web-sovellusten kehittäjien keskuudessa, koska se tarjoaa helpon ja luettavan tavan tallentaa tietoa taulukkoina.

## Miten se toimii?
##### Säännöt:
---
- Käytä kaksoispisteitä avaimina ja arvoina:
    ```
    name: John
    age: 30
    ```
- Käytä välilyöntejä sisennykseen:
    ```
    company:
        name: XYZ Corp
        location: Helsinki
    ```
- Käytä äsken säälittyä merkintätapaa monirivisten tekstimuotojen tallentamisessa:
    ```
    description: |
        Tämä on
        pitkä kuvaus
        useilla riveillä.
    ```

##### Esimerkki:
---
    ```
    // Tiedon tallentaminen YAML-muodossa

    <?php
    $data = yaml_parse_file("data.yaml");
    print_r($data);

    // Tiedon lukeminen YAML-muodosta

    <?php
    $data = array("name" => "Marko", "age" => 25, "company" => array("name" => "ABC Ltd", "location" => "Turku"));
    $output = yaml_emit($data);
    echo $output;
    ```

##### Tuloste:
---
    ```
    Array
    (
        [name] => Marko
        [age] => 25
        [company] => Array
            (
                [name] => ABC Ltd
                [location] => Turku
            )
    )

    name: Marko
    age: 25
    company:
        name: ABC Ltd
        location: Turku
    ```

## Syvemmälle YAML:n maailmaan
YAML kehitettiin alun perin JavaScript Object Notation (JSON) tiedostomuodon vaihtoehdoksi ja se onkin saanut suosiota erityisesti JavaScriptin ympäristössä. JSON on hieman tiukempi muotoilun suhteen, mutta YAML tarjoaa enemmän joustavuutta ja luettavuutta. Joissakin tapauksissa YAML:n käyttö voi kuitenkin hidastaa ohjelman suoritusta verrattuna esimerkiksi XML:ään.

## Katso myös
- [YAML:n virallinen verkkosivusto](http://www.yaml.org/)
- [Symfony YAML-komponentti](https://symfony.com/doc/current/components/yaml.html)