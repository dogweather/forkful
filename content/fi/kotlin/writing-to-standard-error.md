---
title:                "Tietokoneohjelmoinnin artikkeli: Kirjoittaminen normaalivirheelle"
html_title:           "Kotlin: Tietokoneohjelmoinnin artikkeli: Kirjoittaminen normaalivirheelle"
simple_title:         "Tietokoneohjelmoinnin artikkeli: Kirjoittaminen normaalivirheelle"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi

Kirjoittaminen tavallisessa virhevirtaan voi olla hyödyllistä havaitseman virheitä ohjelmoinnissa. Se auttaa myös hahmottamaan, miten ohjelma toimii, kun se on suoritettu.

## Miten

Tässä on yksinkertainen esimerkki siitä, miten kirjoitat virhelookiin Kotlinilla: 
```Kotlin
fun main() {
    println("Kirjoita standardi virhevirtaan")
    System.err.println("Tämä on virheviesti")
}
```

Ja tämä on odotettu tuloste: 
```
Kirjoita standardi virhevirtaan
Tämä on virheviesti
```

## Syvällisempi katsaus

Kotlinilla kirjoittaminen standardi virhevirtaan voidaan tehdä käyttämällä `System.err.println()` -metodia. Tämä metodi tulostaa annetun merkkijonon virhevirtaan ilman rivinvaihtoa. Jos haluat lisätä rivinvaihdon, voit käyttää `System.err.printf()` -metodia ja koodata rivinvaihdon manuaalisesti.

## Katso myös

- [Virheiden käsittely Kotlinissa](https://kotlinlang.org/docs/reference/exceptions.html)
- [Tulostaminen Kotlinilla](https://kotlinlang.org/docs/tutorials/kotlin-for-py/printing-output.html)