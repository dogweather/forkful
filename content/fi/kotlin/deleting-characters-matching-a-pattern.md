---
title:                "Mallin mukaisesti vastaavien merkkien poistaminen"
html_title:           "Kotlin: Mallin mukaisesti vastaavien merkkien poistaminen"
simple_title:         "Mallin mukaisesti vastaavien merkkien poistaminen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi

## Miksi haluat poistaa merkkejä vastaavan mallin
Kun työskentelemme ohjelmointitehtävissä, saatamme törmätä tilanteisiin, joissa haluamme poistaa tietynlaisia merkkejä tietystä merkkijonosta. Tämä voi olla osa tiedonkäsittelyn prosessia tai haluamme ehkä yksinkertaisesti siistiä dataamme. Joko niin, Kotlinilla on helppo tapa poistaa merkkejä, jotka vastaavat tiettyä kaavaa.

## Kuinka tehdä
Tässä on esimerkki siitä, kuinka poistaa kaikki numerot merkkijonosta käyttäen Kotlinin `replace()` -funktiota:

```Kotlin
val merkkijono = "6 hevosta kulkee yli kadun"
val uusiMerkkijono = merkkijono.replace("\\d+".toRegex(), "")
println(uusiMerkkijono)

// Output: "hevosta kulkee yli kadun"
```

## Syvempi sukellus
Kotlin tarjoaa monia hyödyllisiä funktioita merkkijonojen käsittelyyn, kuten `replace()` ja `replaceFirst()`. Näitä funktioita voi käyttää myös poistamaan merkkejä, jotka vastaavat tiettyä kaavaa, kuten olemme esimerkissä tehneet. Voit myös käyttää `removeRange()` -funktiota poistamaan merkkejä tietystä alueesta merkkijonossa. Esimerkiksi `merkkijono.removeRange(2..5)` poistaisi merkit indekseissä 2-5 merkkijonostamme.

## Katso myös
- [Kotlinin virallinen dokumentaatio merkkijonojen manipuloinnista](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/)
- [Kotlinin regular expression -opas](https://kotlinlang.org/docs/regular-expressions.html)
- [How to Remove Specific Characters from a String in Kotlin (Stack Overflow)](https://stackoverflow.com/questions/39983578/how-to-remove-specific-characters-from-a-string-in-kotlin)