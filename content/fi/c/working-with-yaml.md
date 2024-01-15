---
title:                "Työskenteleminen yaml:n kanssa"
html_title:           "C: Työskenteleminen yaml:n kanssa"
simple_title:         "Työskenteleminen yaml:n kanssa"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/working-with-yaml.md"
---

{{< edit_this_page >}}

## Miksi

C-ohjelmoijien kannattaa opetella YAMLin käyttöä, sillä se tarjoaa helpon ja tehokkaan tavan tallentaa ja jakaa tietoa eri ohjelmistojen välillä. YAML on myös laajasti käytetty formaatti konfiguraatiotiedostoissa ja sen oppiminen voi auttaa tehokkaamman ja siistimmän koodin kirjoittamisessa.

## Kuinka

YAML-tiedostot voidaan lukea ja kirjoittaa C-ohjelmassa käyttämällä libyaml-kirjastoa. Alla on esimerkki, jossa luetaan YAML-tiedosto ja tulostetaan sen sisältö konsoliin:

```C
#include <yaml.h>
#include <stdio.h>

int main()
{
    FILE *file = fopen("esimerkki.yaml", "r"); // Avataan tiedosto lukutilaan
    yaml_parser_t parser; // Luodaan parser-objekti
    yaml_document_t document; // Luodaan dokumentti-objekti

    // Alustetaan parser ja liitetään se tiedostoon
    yaml_parser_initialize(&parser);
    yaml_parser_set_input_file(&parser, file);

    // Parsitaan tiedosto ja tallennetaan tulokset dokumentti-objektiin
    if (!yaml_parser_load(&parser, &document))
    {
        printf("Virhe parsittaessa tiedostoa\n");
        return 1;
    }

    // Käydään läpi dokumentti ja tulostetaan sen sisältö
    yaml_node_t *node = yaml_document_get_root_node(&document);
    if (node->type == YAML_MAPPING_NODE)
    {
        yaml_node_pair_t *pair;
        for (pair = node->data.mapping.pairs.start; pair < node->data.mapping.pairs.top; pair++)
        {
            // Tulostetaan avaimen ja arvon tiedot
            printf("Avain: %s\n", yaml_document_get_node(&document, pair->key)->data.scalar.value);
            printf("Arvo: %s\n", yaml_document_get_node(&document, pair->value)->data.scalar.value);
        }
    }

    // Vapautetaan muistinvaraus ja suljetaan tiedosto
    yaml_document_delete(&document);
    yaml_parser_delete(&parser);
    fclose(file);

    return 0;
}
```

Esimerkkitiedoston sisältö:

```yaml
nimi: John Doe
ikä: 30
kieli: C
```

Esimerkkitiedoston tuloste:

```
Avain: nimi
Arvo: John Doe
Avain: ikä
Arvo: 30
Avain: kieli
Arvo: C
```

## Syventymistä

YAML-tiedosto koostuu avain-arvo pareista, joita kutsutaan myös sanakohteiksi (map). YAML tukee myös listoja ja sisäkkäisiä rakenteita. Libyaml-kirjastoa käyttämällä C-ohjelmoijat voivat helposti lukea ja kirjoittaa YAML-tiedostoja ja hyödyntää niitä käyttäjän asetusten, tiedostojen sisällön tai verkkoon liittyvien sovellusten konfiguraatioon.

## Katso myös

- [Libyaml-kirjaston Github-sivu](https://github.com/yaml/libyaml)
- [YAML-lomakekirjasto C:lle](https://github.com/joshgrib/yaml-formatter)
- [YAML-muotomääritys ja esimerkkejä](https://yaml.org/spec/1.2/spec.html)