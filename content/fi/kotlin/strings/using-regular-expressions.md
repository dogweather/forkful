---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:23.588643-07:00
description: "Kuinka: Tarkistaaksesi vastaako merkkijono tietty\xE4 mallia Kotlinissa,\
  \ voit k\xE4ytt\xE4\xE4 `Regex`-luokan `matches`-metodia."
lastmod: '2024-03-13T22:44:56.520852-06:00'
model: gpt-4-0125-preview
summary: "Tarkistaaksesi vastaako merkkijono tietty\xE4 mallia Kotlinissa, voit k\xE4\
  ytt\xE4\xE4 `Regex`-luokan `matches`-metodia."
title: "S\xE4\xE4nn\xF6llisten lausekkeiden k\xE4ytt\xF6"
weight: 11
---

## Kuinka:


### Perusvastaavuuden Tarkistaminen
Tarkistaaksesi vastaako merkkijono tiettyä mallia Kotlinissa, voit käyttää `Regex`-luokan `matches`-metodia.

```kotlin
val pattern = "kotlin".toRegex()
val input = "I love kotlin"
val result = pattern.containsMatchIn(input)

println(result)  // Tuloste: true
```

### Osioiden Etsiminen ja Eksraktointi
Jos haluat etsiä osia merkkijonosta, jotka vastaavat mallia, Kotlin sallii sinun iteroida kaikkien vastaavuuksien yli:

```kotlin
val datePattern = "\\d{2}/\\d{2}/\\d{4}".toRegex()
val input = "Tämän päivän päivämäärä on 07/09/2023."
val dates = datePattern.findAll(input)

for (date in dates) {
    println(date.value)
}
// Tuloste: 07/09/2023
```

### Tekstin Korvaaminen
Merkkijonon osien korvaaminen, jotka vastaavat mallia, on suoraviivaista `replace`-funktion avulla:

```kotlin
val input = "Username: user123"
val sanitizedInput = input.replace("\\d+".toRegex(), "XXX")

println(sanitizedInput)  // Tuloste: Username: userXXX
```

### Merkkijonojen Jakaminen
Jaa merkkijono listaan käyttäen regex-mallia erotinmerkkinä:

```kotlin
val input = "1,2,3,4,5"
val numbers = input.split(",".toRegex())

println(numbers)  // Tuloste: [1, 2, 3, 4, 5]
```

### Kolmannen Osapuolen Kirjastot: Kotest
[Kotest](https://github.com/kotest/kotest) on suosittu Kotlin-testauskirjasto, joka laajentaa Kotlinin sisäänrakennettua regex-tukea, erityisesti hyödyllinen validoinnissa testitapauksissa.

```kotlin
// Olettaen, että Kotest on lisätty projektiisi
import io.kotest.matchers.string.shouldMatch

val input = "kotlin@test.com"
input shouldMatch "\\S+@\\S+\\.com".toRegex()

// Tämä läpäisee testin, jos syöte vastaa sähköpostimallia.
```

Sisällyttämällä säännöllisiä lausekkeita Kotlin-sovelluksiisi, voit suorittaa monimutkaista tekstinkäsittelyä tehokkaasti. Olipa kyseessä käyttäjän syötteen validointi, datan eksraktointi tai merkkijonojen muuntaminen, regex-mallit tarjoavat vankan ratkaisun.
