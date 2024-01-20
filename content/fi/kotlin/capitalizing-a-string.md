---
title:                "Merkkijonon isoksi kirjoittaminen"
html_title:           "Kotlin: Merkkijonon isoksi kirjoittaminen"
simple_title:         "Merkkijonon isoksi kirjoittaminen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Merkkijonojen pääomittaminen tarkoittaa kaikkien merkkijonojen ensimmäisten kirjainten muuttamista isoiksi kirjaimiksi. Tämä on hyödyllistä, kun haluamme esittää tekstin tietyssä formaatissa tai tehdä tekstin tunnistamisen helpommaksi.

## Miten:

Voit pääomittaa merkkijonon Kotlinissa seuraavasti:

```Kotlin
    val nimi = "esimerkki"
    val pääomitettyNimi = nimi.capitalize()
    println(pääomitettyNimi)  // Tulostaa: Esimerkki
```

## Syvempi sukellus:

Merkkijonojen pääomittaminen on ollut ohjelmointikielissä jo pitkään, ja sen toteutustavat voivat vaihdella. Kotlinin `capitalize()` funktio vaikuttaa vain merkkijonon ensimmäiseen merkkiin, ja se palauttaa uuden merkkijonon.

Vaihtoehtoisia tapoja on olemassa, esimerkiksi voit käyttää `toUpperCase()` metodia jokaiseen merkkiin merkkijonossa:

```Kotlin
    val nimi = "esimerkki"
    val pääomitettyNimi = nimi.toUpperCase()
    println(pääomitettyNimi)  // Tulostaa: ESIMERKKI
```

Voit myös käyttää `Locale` luokkaa sovelluksissa, joissa tarvitset kielikohtaisia pääomituksia, koska jotkut kielet käsittelevät pääomituksen eri tavalla.

## Katso myös:

1. Kotlinin virallinen ohjeka: [Kotlin Documentation](https://kotlinlang.org/docs/reference/)

2. [capitalize()](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/capitalize.html)

3. [toUpperCase()](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/to-upper-case.html)

4. [Locale](https://developer.android.com/reference/java/util/Locale)