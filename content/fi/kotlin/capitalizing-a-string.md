---
title:                "Aloita merkkijonon kirjoitus suurella alkukirjaimella"
html_title:           "Kotlin: Aloita merkkijonon kirjoitus suurella alkukirjaimella"
simple_title:         "Aloita merkkijonon kirjoitus suurella alkukirjaimella"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Miksi ohjelmoijat käyttävät isojentain kirjainten kirjoittamista? Kyseessä on yksinkertainen tapa muuttaa merkkijonoa niin, että jokainen sana alkaa isolla kirjaimella. Tämä tekee tekstin lukemisesta helpompaa ja visuaalisesti miellyttävämpää.

## Miten:
 
```Kotlin
val name = "kotlin on loistava ohjelmointikieli"
println(name.capitalize())
//Tulostaa: "Kotlin on loistava ohjelmointikieli"
```

Capitalizingia on myös mahdollista käyttää merkkijonon osien muokkaamiseen. Tarkemmat ohjeet löytyvät [täältä](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/capitalize.html).

## Syvemmälle:
Isoksi kirjoittamisen tekniikki on ollut käytössä jo pitkään, sillä se on ollut tapana jo vanhoissa kirjoitustyyleissä. Nykyään sitä käytetään esimerkiksi otsikoissa ja nimissä. On myös olemassa muita tapoja muuttaa merkkijonoja, esimerkiksi käyttämällä funktiota `toUpperCase()`, mutta capitalizing on yleisesti helpommin ymmärrettävä ja käytetympi vaihtoehto.

## Katso myös:
- [Kotlinin virallinen dokumentaatio merkkijonojen käsittelystä](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/)
- [Funktion `capitalize()` dokumentaatio](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/capitalize.html)