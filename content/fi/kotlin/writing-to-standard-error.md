---
title:                "Kotlin: Standardivirheen kirjoittaminen"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi kirjoittaa standard error -virtaan?

Kirjoittaminen standard error -virtaan on hyödyllistä silloin, kun haluat saada tarkempaa tietoa ohjelmasi toiminnasta tai virhetilanteista. Se on myös kätevä tapa seurata koodin suorittamista ja mahdollistaa virheiden helpomman jäljittämisen.

## Kuinka tehdä se Kotlinilla?

Kotlinissa voidaan käyttää standardin kirjastoa `System.err` ja sen metodia `println()` virheilmoitusten kirjoittamiseen. Esimerkiksi:

```Kotlin
System.err.println("Virheilmoitus tähän.")
```

Tämä tulostaa virheilmoituksen standard error -virtaan ja se voidaan sitten lukea esimerkiksi konsolilta tai virhelokeista.

## Syvempi sukellus

Kun kirjoitat standard error -virtaan, tiedot tallentuvat erilliseen virtaan kuin tavalliset tulostukset. Tämä mahdollistaa virheilmoitusten erottamisen muusta ohjelman tulostuksesta ja helpottaa virheiden tunnistamista ja korjaamista.

On myös tärkeää huomata, että standard error -virtaa ei tyhjennetä automaattisesti ohjelman suorituksen jälkeen, joten tarvittaessa se on puhdistettava käsin.

## Katso myös

- [Kotlinin virallinen dokumentaatio System.err:stä](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-system.err/)
- [Jenkovin blogipostaus virheilmoitusten kirjoittamisesta standard error -virtaan Java-sovelluksissa](http://tutorials.jenkov.com/java-io/system-out-err.html)