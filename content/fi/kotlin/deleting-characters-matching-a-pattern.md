---
title:                "Merkkien poistaminen vastaavalla mallilla"
html_title:           "Arduino: Merkkien poistaminen vastaavalla mallilla"
simple_title:         "Merkkien poistaminen vastaavalla mallilla"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Hahmon poistaminen tarkoittaa tiettyjen merkkien tai merkityyppien poistamista tekstistä. Ohjelmoijat tekevät tämän monestakin syystä, esimerkiksi datan siivoamiseksi tai tekstissä olevan tiedon uuttamiseksi.

## Miten näin:

Kotlinin sisäisten funktioiden, kuten replace() tai replaceAll(), avulla voimme poistaa hahmot. Katsotaan esimerkkiä.

```Kotlin
val str = "Tervetuloa Kotlinin maailmaan!"
val result = str.replace("o", "")
println(result) // Tulostaa "Tervetulaa Kotlinin maailmaan!"
```
Tässä esimerkissä poistamme kaikki "o" kirjaimet merkkijonosta.

## Sukellus syvemmälle

Historiallisesti merkkien poistaminen on ollut olennainen osa tekstinkäsittelyohjelmoinnin sekä datankäsittelyn. Nykyisin Kotlinin tarjoamia valmiita toimintoja kannattaa käyttää.

Vaihtoehtoina voit myös hyödyntää muita tekniikoita kuten Regular Expression -ilmaisuja (regex). Niiden avulla voit määritellä monimutkaisempia malleja poistettavaksi.

```Kotlin
val str = "Terve 123, tervetuloa Kotlinin maailmaan 456!"
val result = str.replace("\\d+".toRegex(), "")
println(result) // Tulostaa "Terve , tervetuloa Kotlinin maailmaan !"
```

Tässä esimerkissä käytämme regex lauseketta "\\d+", joka tunnistaa yhden tai useamman numerosarjan ja poistaa ne merkkijonosta.

## Katso myös

1. [Kotlinin virallinen dokumentaatio replace()-funktiosta](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/replace.html)
2. [Tietoa säännöllisistä lausekkeista (regex) Kotlinissa](https://www.programiz.com/kotlin-programming/regex)
3. [Kattava opas regex-syntaksiin](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)