---
title:                "Merkkijonon interpolointi"
html_title:           "Bash: Merkkijonon interpolointi"
simple_title:         "Merkkijonon interpolointi"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Merkinjonojen interpolointi tarkoittaa muuttujien sisällyttämistä suoraan merkkijonoihin. Ohjelmoijat käyttävät sitä saadakseen siistimmän ja ytimekkäämman koodin.

## Näin se tehdään:
Kotlinissa merkkijonojen interpolointi on helppoa. Katsotaan esimerkkiä.

```Kotlin
fun main() {
    val nimi = "Kotlin"
    println("Hei, olen $nimi")
}
```

Tuotanto:

```Kotlin
Hei, olen Kotlin
```

Jos haluat käyttää kokonaisen lausekkeen, käytä sulkeita (`{}`):

```Kotlin
fun main() {
    val x = 10
    val y = 20
    println("Summa on ${x + y}")
}
```

Tuotanto:

```Kotlin
Summa on 30
```

## Syvemmälle Sukeltaminen
Merkkijonojen interpolointi sai alkunsa alun perin Perl-ohjelmointikielestä ja on sen jälkeen sisällytetty useisiin ohjelmointikieliin, mukaan lukien Kotlin. Kotlinissa voit myös käyttää merkkijonojen formaattia, joka on Java-tyylinen vaihtoehto, mutta merkkijonojen interpolointi on tyypillisesti siistimpää ja selkeämpää.

```kotlin
val nimi = "Kotlin"
val viesti = String.format("Hei, olen %s", nimi)
println(viesti)
```
Tämän merkkijono interpoloinnin toteutus Kotlinissa tapahtuu kääntäjätasolla, missä interpoloidut merkkijonot muunnetaan sopiviin `StringBuilder` -operaatioihin.

## Katso myös
- [Kotlinin virallinen dokumentaatio merkkijonojen interpoloinnista](https://kotlinlang.org/docs/strings.html#string-templates)
- [Merkkijonojen interpolointi muissa ohjelmointikielissä](https://en.wikipedia.org/wiki/String_interpolation)