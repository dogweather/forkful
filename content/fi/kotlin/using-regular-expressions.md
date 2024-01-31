---
title:                "Säännöllisten lausekkeiden käyttö"
date:                  2024-01-19
simple_title:         "Säännöllisten lausekkeiden käyttö"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Regulaarilausekkeet ("regex") on työkalu merkkijonojen käsittelyyn. Niiden avulla voimme suodattaa, korvata tai tarkistaa merkkijonot kuvioiden mukaan – nopeasti ja tehokkaasti.

## How to:
Käytetään regexiä Kotlinissa. Piirretään esimerkki merkkijonon tarkistuksesta ja korvaamisesta.

```Kotlin
fun main() {
    val teksti = "Kotlin on huippu 2023!"
    
    // Tarkistetaan onko vuosi merkkijonossa
    val vuosiRegex = """\b\d{4}\b""".toRegex()
    println(vuosiRegex.containsMatchIn(teksti)) // Tulostaa: true
    
    // Korvataan "huippu" sanalla "mahtava"
    val korvaus = teksti.replace("huippu", "mahtava")
    println(korvaus) // Tulostaa: Kotlin on mahtava 2023!
}
```

## Deep Dive
Regulaarilausekkeet ovat peräisin 1950-luvulta, Stephen Kleenen teoriasta. Regexin vaihtoehtoja on lukuisia: saatat käyttää `String`-metodeja kuten `contains` tai `startsWith`. Kotlinin `Regex`-luokka käyttää alaisuudessaan Javan `Pattern`- ja `Matcher`-luokkia, joten sen suorituskyky on vertailukelpoinen.

## See Also
- [Kotlinin virallinen dokumentaatio regexistä](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)
- [RegExr](https://regexr.com/): interaktiivinen työkalu regexien harjoitteluun ja testaamiseen
- [RegEx101](https://regex101.com/): toinen vaihtoehto regexien testaamiseen online.
