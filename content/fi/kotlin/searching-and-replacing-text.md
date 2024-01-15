---
title:                "Tekstin etsiminen ja korvaaminen"
html_title:           "Kotlin: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit etsiä ja korvata tekstiä ohjelmointikielessä? Eikö se ole vain yksinkertaisempi tehdä manuaalisesti? No, etsintä ja korvaaminen voi olla erittäin hyödyllistä, jos sinun tarvitsee muuttaa useita esiintymiä saman tekstin sisällä tai jos haluat tehdä sen automaattisesti. Tämä säästää aikaa ja vaivaa, ja on erityisen hyödyllistä, jos työskentelet suurten tietomäärien parissa.

## Miten Sillä

Etsintä ja korvaaminen Kotlinissa on helppoa ja tehokasta. Voit käyttää `replace()` -funktiota ja antaa sille kaksi parametria: etsittävä teksti ja korvaava teksti. Tämä korvaa kaikki esiintymät etsitystä tekstistä korvaavalla tekstillä.

```Kotlin
val string = "Tämä on esimerkkilause."

val uusiString = string.replace("esimerkki", "uusi")
// uusiString: Tämä on uusi lause.
```

Jos haluat tehdä korvauksen vain tietyille esiintymille, voit antaa kolmannen parametrin `replace()` -funktiolle, joka määrittää korvattavan esiintymän indeksin.

```Kotlin
val string = "Hei maailma, hei Kotlin!"

val uusiString = string.replace("hei", "Hello", 1)
// uusiString: Hello maailma, hei Kotlin!
```

Voit myös käyttää `replaceAll()` -funktiota, joka korvaa kaikki tekstin esiintymät annetulla korvaavalla tekstillä.

```Kotlin
val string = "Tämä on esimerkki lause, esimerkki lause."

val uusiString = string.replaceAll("esimerkki", "uusi")
// uusiString: Tämä on uusi lause, uusi lause.
```

## Syväsukellus

Kotlin tarjoaa monia vaihtoehtoja etsintään ja korvaamiseen tekstissä. Voit käyttää myös säännöllisiä lausekkeita `Regex` -luokan avulla. Tämä mahdollistaa monimutkaisemmat ja tarkemmat etsinnät.

```Kotlin
val string = "Tämä on esimerkkilause 1. Tämä on esimerkkilause 2."

val pattern = Regex("esimerkki(\\d+)")

val uusiString = pattern.replace(string, "uusi$1")
// uusiString: Tämä on uusi lause 1. Tämä on uusi lause 2.
```

Voit myös käyttää `replaceFirst()` ja `replaceLast()` -funktioita, jos haluat korvata vain ensimmäisen tai viimeisen esiintymän annetulla korvaavalla tekstillä.

```Kotlin
val string = "Hei maailma, hei Kotlin!"

val uusiString = string.replaceFirst("hei", "Hello")
// uusiString: Hello maailma, hei Kotlin!

val uusiString2 = string.replaceLast("hei", "Hello")
// uusiString2: Hei maailma, Hello Kotlin!
```

## Katso Myös

- [Kotlinin merkkijonomenetelmät](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string/#manipulation) (Kotlinin virallinen dokumentaatio)
- [Java Regular Expressions](https://docs.oracle.com/javase/10/docs/api/java/util/regex/Pattern.html) (Java:n virallinen dokumentaatio säännöllisistä lausekkeista)