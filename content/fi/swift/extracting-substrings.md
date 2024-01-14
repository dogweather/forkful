---
title:                "Swift: Alimerkkijonojen erottaminen"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi: Substringien erottamisen hyödyt

Substringit eli osajonot ovat tärkeitä osia monissa ohjelmoinnin tehtävissä, kuten tekstinkäsittelyssä ja tiedon etsimisessä. Substringien erottaminen mahdollistaa tietyn osan tekstin käsittelyä erikseen, ja siten helpottaa monimutkaisia ohjelmointitehtäviä.

## Näin teet sen: Koodiesimerkkejä ja tulosteita

```Swift
let teksti = "Tämä on esimerkki tekstistä."
let alku = teksti.index(teksti.startIndex, offsetBy: 5)
let loppu = teksti.index(teksti.endIndex, offsetBy: -7)
let osajono = teksti[alku..<loppu]

print(osajono) // tulostaa "on esimerkki teksti"
```

Yllä oleva koodiesimerkki osoittaa, miten voit käyttää `index` -funktiota määrittämään halutun substringin alku- ja loppukohta. Tämän jälkeen voit käyttää `range` toimintoa erottamaan kyseisen osajonon ja tulostamaan sen.

## Sukella syvemmälle: Substringien erottamisen tiedostaminen

Substringien erottamisessa on tärkeää kiinnittää huomiota alku- ja loppupisteiden määrittämiseen, sillä virheellisillä koodeilla voi olla merkittäviä vaikutuksia lopullisiin tuloksiin. Lisäksi on tärkeää ymmärtää, miten `index` ja `range` -funktiot toimivat, jotta voit käyttää niitä tehokkaasti.

## Katso myös

* [Strings - Apple Developer Documentation](https://developer.apple.com/documentation/swift/strings)
* [Substring - Apple Developer Documentation](https://developer.apple.com/documentation/swift/substring)