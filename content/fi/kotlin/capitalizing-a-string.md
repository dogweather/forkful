---
title:    "Kotlin: Merkkijonon kirjoittaminen isolla alkukirjaimella"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Miksi

Joskus on tarpeen muuttaa merkkijonon kirjaimia isoiksi kirjaimiksi. Tähän voi olla erilaisia syitä, kuten siistimmän ulkoasun luominen tai tietyn datan käsittelyn helpottaminen. Kotlin tarjoaa valmiin funktion, joka mahdollistaa tämän toiminnon suorittamisen helposti.

## Kuinka

Seuraavassa on esimerkkejä siitä, kuinka muuttaa merkkijonon kirjaimia isoiksi kirjaimiksi Kotlinilla.

```Kotlin
val name = "anna" // alkuperäinen merkkijono
val capitalized = name.toUpperCase() // merkkijonon muuttaminen isoiksi kirjaimiksi
println(capitalized) // tulostaa "ANNA"
```

Voit myös käyttää `capitalize()` funktiota muuttaaksesi vain ensimmäisen kirjaimen isoksi.

```Kotlin
val name = "anna" // alkuperäinen merkkijono
val capitalized = name.capitalize() // muuttaa vain ensimmäisen kirjaimen isoksi
println(capitalized) // tulostaa "Anna"
```

## Syvällinen sukellus

Kotlinissa merkkijonot ovat luonteeltaan immutaabeleja, mikä tarkoittaa sitä, että niitä ei voida muuttaa alkuperäisen luomisen jälkeen. Tämän vuoksi `capitalize()` ja `toUpperCase()` funktiot tuottavat uuden merkkijonon, eikä muutokset vaikuta alkuperäiseen.

Lisäksi, jos käytät `toUpperCase()` funktiota, se muuttaa kaikki kirjaimet isoksi, mutta `capitalize()` funktio muuttaa vain ensimmäisen kirjaimen isoksi ja muut säilytetään alkuperäisinä.

## Katso myös

- [Kotlinin virallinen dokumentaatio merkkijonojen käsittelystä](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/index.html)
- [Kotlinin merkkijonojen käsittelyä käsittelevä youtube-video](https://www.youtube.com/watch?v=kz_o6TRy5Oo)
- [Kotlinin merkkijonojen käyttäminen String Literaleissa](https://www.codeproject.com/tips/1255096/string-handling-in-kotlin-part-2-plusing-string-l?display=Print)