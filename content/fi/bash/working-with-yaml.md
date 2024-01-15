---
title:                "Yamlin käyttäminen"
html_title:           "Bash: Yamlin käyttäminen"
simple_title:         "Yamlin käyttäminen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/working-with-yaml.md"
---

{{< edit_this_page >}}

## Miksi

YAML on helppo ja intuitiivinen tapa tallentaa ja jakaa tietoa eri ohjelmien ja sovellusten välillä. Se on myös hyvin yleisesti käytetty formaatti konfiguraatiotiedostoissa ja muissa tietojen tallennuksessa. 

## Miten

Jos haluat aloittaa työskentelyn YAML:n kanssa, sinun tarvitsee ainoastaan asentaa Bash ja sen mukana tuleva yamlin paketti. Voit käynnistää yamlin käyttämällä komentoa “yamlin” ja kutsumalla sitä Bash-skriptissä käyttämällä komentoa “yamlin <tiedostonimi>”. Alla on yksinkertainen esimerkki, joka näyttää kuinka tallentaa ja lukea YAML-tiedostoa yamlin avulla:

```Bash
#!/bin/bash

# Tallennetaan YAML-tiedostoon tietoja
yamlin testi.yaml set "nimi: John"
yamlin testi.yaml set "ikä: 30"

# Luetaan YAML-tiedostosta ja tulostetaan tiedot
nimi=$(yamlin testi.yaml get nimi)
ikä=$(yamlin testi.yaml get ikä)
echo "Käyttäjän $nimi ikä on $ikä vuotta."
```

Esimerkissä luodaan YAML-tiedosto nimeltä "testi.yaml" ja tallennetaan siihen kaksi tietoa: nimi ja ikä. Tämän jälkeen tiedot luetaan ja tulostetaan Bash-konsolissa. Tulostuksena näemme "Käyttäjän John ikä on 30 vuotta."

## Syvempää sukellusta

Yamlin avulla on mahdollista muokata ja manipuloida YAML-tiedostoja eri tavoin. Voit esimerkiksi käyttää “update”-komennon avulla muuttamaan nykyistä tietoa tai lisäämään uusia tietoja YAML-tiedostoon. Voit myös poistaa tietoja käyttämällä “delete”-komentoa.

Lisäksi yamlin avulla voit myös yhdistää useita YAML-tiedostoja yhdeksi kokonaisuudeksi käyttämällä “merge”-toimintoa. Tämä helpottaa esimerkiksi eri ympäristöjen konfiguraatioiden hallintaa.

## Katso myös

- YAML: https://yaml.org/
- YAML-tiedostomuoto: https://en.wikipedia.org/wiki/YAML
- Yamlint: https://github.com/adrienverge/yamlicious