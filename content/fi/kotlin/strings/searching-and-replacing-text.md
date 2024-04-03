---
date: 2024-01-20 17:58:22.777526-07:00
description: "Tekstin etsiminen ja korvaaminen on yksinkertaisesti tekstijonon vaihtamista\
  \ toiseen. Koodareille t\xE4m\xE4 on t\xE4rke\xE4\xE4, koska koodin muokkaaminen,\
  \ virheiden\u2026"
lastmod: '2024-03-13T22:44:56.516352-06:00'
model: gpt-4-1106-preview
summary: Tekstin etsiminen ja korvaaminen on yksinkertaisesti tekstijonon vaihtamista
  toiseen.
title: Tekstin etsiminen ja korvaaminen
weight: 10
---

## What & Why? (Mitä & Miksi?)
Tekstin etsiminen ja korvaaminen on yksinkertaisesti tekstijonon vaihtamista toiseen. Koodareille tämä on tärkeää, koska koodin muokkaaminen, virheiden korjaaminen ja datan muokkaus vaativat usein nopeita tekstioperaatioita.

## How to: (Kuinka tehdä:)
Kotlinissa tekstiä voi etsiä ja korvata `replace`-funktiolla. Alla on esimerkkejä käytännössä:

```Kotlin
fun main() {
    val originalText = "Kotlin on huippukieli!"
    val newText = originalText.replace("huippukieli", "mahtava kieli")
    println(newText) // Tulostaa: Kotlin on mahtava kieli!
}
```

Jos tarvitset tarkempaa haku- ja korvauskriteeriä, käytä regexiä:

```Kotlin
fun main() {
    val regexPattern = "[0-9]+".toRegex()
    val address = "Osoite: Puistokatu 1234"
    val sanitizedAddress = address.replace(regexPattern, "****")
    println(sanitizedAddress) // Tulostaa: Osoite: Puistokatu ****
}
```

## Deep Dive (Sukellus syvemmälle)
Tekstin etsiminen ja korvaaminen juontaa juurensa varhaisiin tekstieditoreihin ja prosessointityökaluihin, kuten `sed` UNIX-järjestelmissä. Kotlinissa `replace` käsittää kaksi varianttia: yksi yksinkertaisille merkkijonoille ja toinen säännönmukaisille lausekkeille (regex). Ole tarkkana regexin kanssa, ne voivat olla tehokkaita mutta myös kalliita suorituskyvylle jos niitä käytetään väärin.

Vaihtoehtona `replace`-funktiolle, voit käyttää `StringBuilder`-luokkaa muokataksesi merkkijonoja suorituspaikan päällä, mikä voi olla tehokkaampaa isommissa teksteissä.

## See Also (Katso myös)
- Kotlin-dokumentaatio `replace`-funktiosta: [Kotlin replace](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/replace.html)
- Kotlin regex-oppaat: [Kotlin Regex](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)
- Wikipedian artikkeli säännönmukaisista lausekkeista: [Regular Expressions](https://en.wikipedia.org/wiki/Regular_expression)
