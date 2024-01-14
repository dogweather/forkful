---
title:                "C: Työskentely yaml:n kanssa"
simple_title:         "Työskentely yaml:n kanssa"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/working-with-yaml.md"
---

{{< edit_this_page >}}

## Miksi

YAML on tänä päivänä yksi suosituimmista tiedostomuodoista tietojen tallentamiseen ja jakamiseen. Se tarjoaa yksinkertaisen ja ihmislähtöisen syntaksin, joka on helppo lukea ja ymmärtää. Tämän ansiosta se on suosittu valinta monissa ohjelmointiprojekteissa, kuten C-ohjelmoinnissa.

## Miten

YAML-tiedostojen käsittely C-ohjelmoinnissa on helppoa ja suoraviivaista. Tässä esimerkissä käytämme "libyaml" -kirjastoa, joka tarjoaa toimintoja YAML-tiedostojen lukemiseen ja kirjoittamiseen. Seuraavassa koodilohkossa näytämme, kuinka lähetämme YAML-tiedoston konsolilta ja tulostamme sen sisällön näytölle.

```C
#include <stdio.h>
#include <yaml.h>

int main() {
    // avataan YAML-tiedosto
    FILE *file = fopen("esimerkki.yaml", "r");
    
    // määritetään YAML-tiedoston puskuri
    yaml_parser_t parser;
    yaml_parser_initialize(&parser);
    yaml_parser_set_input_file(&parser, file);
    
    // luetaan tiedostosta yksi YAML-tapahtuma
    yaml_event_t event;
    yaml_parser_parse(&parser, &event);
    
    // tulostetaan tapahtuman sisältö näytölle
    printf("Tapahtuman tyyppi: %d\n", event.type);
    printf("Tapahtuman sisältö: %s\n", event.data.scalar.value);
    
    // suljetaan tiedosto ja lopetetaan ohjelma
    fclose(file);
    return 0;
}
```

Käytämme tässä vain yhtä tapahtumaa esimerkkinä, mutta voit käyttää yaml_parser_parse -funktiota lukemaan koko tiedoston sisällön. Lisätietoja "libyaml":sta löydät "See Also" -kohdasta.

## Syventyminen

Vaikka YAML:n käyttö C-ohjelmoinnissa on yksinkertaista, kannattaa tuntea tiedostomuodon ominaisuudet ja rajaukset paremmin. YAML käyttää sisennystä merkkinä rakenteen muodostamiseksi, joten on tärkeää ymmärtää, kuinka tarkasti sisennykset ovat määritelty. Lisäksi on hyvä tietää, mitä tietotyyppejä YAML tukee ja kuinka niitä käsitellään ohjelmassa.

## Katso myös

- [libyaml: C-kielen YAML lukija/kirjoittaja](https://pyyaml.org/wiki/LibYAML)
- [YAML:n viralliset dokumentaatiot](https://yaml.org/)
- [YAML-tiedoston syntaksin esittely](https://www.w3schools.io/file/yaml-syntax-cheatsheet/)