---
title:                "Tekstin etsiminen ja korvaaminen"
html_title:           "Arduino: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Kotlinilla Tekstin Etsintä ja Korvaus: Opas 

## Mikä & Miksi?

Tekstin etsiminen ja korvaaminen on tapa muuttaa pyydettyä jonoa, korvaamalla se toisella. Ohjelmoijat tekevät tämän yleisesti kun heidän tulee muokata tai käsittellä jonoja tarkoituksenmukaisimmaksi - esimerkiksi, poistamaan alusta tai päästä sanoja tai korvata spesifiset sanat toisilla.

## Näin teet:

```Kotlin
val lause = "Tervetuloa Kotlinin maailmaan!"
val korvattuLause = lause.replace("Kotlinin", "Ohjelmoinnin")
println(korvattuLause)
```

Tämän koodin tuloste on: "Tervetuloa Ohjelmoinnin maailmaan!".

## Syvä sukellus:

Tekstin etsiminen ja korvaaminen on ollut olemassa melkein yhtä kauan kuin ohjelmointi itsessään. Kotlin tarjoaa helpon tavan sen toteuttamiseksi, mutta on hyvä pitää mielessä, että tämä ei ole ainoa tapa. Myös regex-funktiota (säännöllisiä lausekkeita) voidaan käyttää monimutkaisempiin tekstinkäsittelytehtäviin.

Tämä toiminto toimii etsimällä tietoa jokaisesta jonon osasta, kunnes se löytää vastaavuuden, ja sitten se korvaa sen. Kaikkien toimintojen tarkkuus ja nopeus riippuvat siitä, miten hyvin ohjelma on rakennettu ja kuinka hyvin ohjelmointikieli tukee näitä toimintoja.

## Katso myös:

1. Tekstin käsittely Kotlissa: [Linkki](https://kotlinlang.org/docs/text-operations.html)
2. Säännölliset lausekkeet Kotlinissa: [Linkki](https://kotlinlang.org/docs/regular-expressions.html)
3. Tietoa Kotlinin `replace`-funktiosta: [Linkki](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/replace.html)

---

Muista, että hyvät ohjelmat ovat aina niitä, jotka ovat hyvin suunniteltuja ja jotka ottavat huomioon yksityiskohdat. Kotlin tarjoaa paljon työkaluja, jotka auttavat tekemään tekstinkäsittelystä tehokkaampaa ja tuottavampaa. Aina on hyvä aika oppia jotain uutta!