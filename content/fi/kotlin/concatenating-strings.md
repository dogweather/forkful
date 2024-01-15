---
title:                "Stringien yhdistäminen"
html_title:           "Kotlin: Stringien yhdistäminen"
simple_title:         "Stringien yhdistäminen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi

Stringien ketjuttaminen on yleinen ohjelmointitekniikka, joka mahdollistaa useiden merkkijonojen yhdistämisen yhdeksi merkkijonoksi. Tämä on erityisen hyödyllistä, kun haluat luoda dynaamisia tekstilausekkeita tai luoda otsikkotekstejä.

## Kuinka

Stringien ketjuttamiseen voit käyttää Kotlinin sisäänrakennettua `plus()`-funktiota tai käyttää `+=`-operaattoria.

```Kotlin
// Example 1
val nimi = "Matti"
val tervehdys = "Hei, $nimi!"

println(tervehdys)

// Output:
// Hei, Matti!

// Example 2
val aloitus = "Tervetuloa"
val paikka = "Kotisivuillemme"
val otsikko = "$aloitus $paikka!"

println(otsikko)

// Output:
// Tervetuloa Kotisivuillemme!
```

On myös mahdollista ketjuttaa useampia merkkijonoja samassa lausekkeessa.

```Kotlin
// Example
val opiskelija = "Maija"
val kurssi = "ohjelmointi"
val opintojakso = "kieli"
val lause = "$opiskelija opiskelee $kurssi $opintojaksoa."

println(lause)

// Output:
// Maija opiskelee ohjelmointi kieltä.
```

## Syvempää tietoa

Kotlinissa on myös mahdollista käyttää `StringBuilder`-luokkaa merkkijonojen ketjuttamiseen. Tämä on tehokkaampi vaihtoehto, kun haluat ketjuttaa suuremman määrän merkkijonoja.

```Kotlin
// Example
val text = StringBuilder("Kissa")
text.append("nimi").append(" on Mimi.")

println(text)

// Output:
// Kissanimi on Mimi.
```

On myös tärkeää huomata, että ketjutetut merkkijonot ovat muuttumattomia, eli niitä ei voi muokata jälkikäteen. Jokaisen ketjutuksen jälkeen muodostetaan uusi merkkijono, ja alkuperäiset merkkijonot säilyvät muuttumattomina.

## Katso myös

- [Kotlinin dokumentaatio](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [10 things to love about Kotlin](https://www.infoworld.com/article/3274053/10-things-to-love-about-kotlin.html)