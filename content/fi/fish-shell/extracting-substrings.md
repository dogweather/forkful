---
title:                "Alirivien erottaminen"
html_title:           "Fish Shell: Alirivien erottaminen"
simple_title:         "Alirivien erottaminen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Substringien erottelu on tapa organisoida tekstimuotoista dataa osiin, joita voidaan käsitellä erillisinä kokonaisuuksina. Tämä helpottaa datan käsittelyä ja tarjoaa mahdollisuuden suorittaa erilaisia toimintoja tiettyjen osien suhteen. Koodaajat käyttävät substringien erottelua usein tietyn tiedon tunnistamiseen ja käsittelemiseen, kuten esimerkiksi tiedostonimiä tai URL-osoitteita.

## Miten:

Fish Shell -ohjelmointikielen avulla substringien erottelu on helppoa ja nopeaa. Alla on esimerkkejä siitä, miten voit erottaa substringin tekstistä ja tulostaa sen konsoliin.

```
Fish Shell -koodia:

set input "Tämä on tekstiä"
echo (echo $input | cut -d ' ' -f 2)
```

Tuloste: "on"

Voit myös erottaa useamman substringin samalla kertaa ja tulostaa ne konsoliin. Esimerkiksi:

```
Fish Shell -koodia:

set input "John Doe 30 vuotta"
echo (string split "JohnDoe30 vuotta" | cut -d ' ' -f 1,2)
```

Tuloste: "John Doe"

Fish Shell -kielen substring-funktioilla on myös muita hyödyllisiä vaihtoehtoja, kuten säännöllisiä lausekkeita ja merkkijonojen vaihtamista. Näitä voit käyttää vielä tarkempien substringien erotteluun.

## Syväsukellus:

Substringien erottelu on ollut osa ohjelmointikieliä jo pitkään, ja sen suosio johtuu sen käytännöllisyydestä ja monipuolisuudesta. Fish Shellin lisäksi myös muut ohjelmointikielet, kuten Python ja Java, tarjoavat erilaisia työkaluja ja algoritmeja substringien erotteluun.

Fish Shellin substring-toiminto perustuu usein Unix-käyttöjärjestelmän cut-komentoon, joka tarjoaa erilaisia vaihtoehtoja tietyn merkkijonon erotteluun. Tämä mahdollistaa myös erilaisten komentoriviohjelmien integroinnin Fish Shellin substring-toimintoihin.

## Katso myös:

Löydät lisätietoa Fish Shellin substring-toiminnoista Fish Shell -ohjelmointikielen dokumentaatiosta: https://fishshell.com/docs/2.7/cmds/set.html#string-split. Voit myös tutustua muihin ohjelmointikieliin ja niiden substring-toimintoihin, kuten Pythonin string-joukkoon: https://docs.python.org/3/library/stdtypes.html#string-methods.