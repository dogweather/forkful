---
title:                "Komentoriviparametrien lukeminen"
html_title:           "Kotlin: Komentoriviparametrien lukeminen"
simple_title:         "Komentoriviparametrien lukeminen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Komentoriviargumenttien lukeminen on tapa, jolla ohjelmoijat voivat lukea syötteitä, joita käyttäjä antaa suoraan ohjelmalle komentorivillä. Tämä voi olla hyödyllistä esimerkiksi ohjelmassa, joka käsittelee tiedostoja, sillä käyttäjä voi antaa tiedoston nimen komentorivillä sen sijaan, että sen täytyisi kirjoittaa se ohjelman käyttöliittymään.

## Kuinka:

```Kotlin
fun main(args: Array<String>) {
    println("Ensimmäinen argumentti: ${args[0]}")
    println("Toinen argumentti: ${args[1]}")
}
```

```
kotlin run tiedosto.kt argumentti1 argumentti2
```

Tässä esimerkissä ohjelma tulostaa ensimmäisen ja toisen komentoriviargumentin arvon. Vaikka tässä esimerkissä käytetään vain kahta argumenttia, samaa logiikkaa voi soveltaa mihin tahansa määrään argumentteja.

## Syvällinen kaivelu:

Komentoriviargumenttien lukeminen on ollut käytössä jo vuosikymmenten ajan, ja se on edelleen yksi yleisimmistä tavoista käsitellä käyttäjän syötteitä ohjelmissa. On myös muita tapoja lukea syötteitä, kuten esimerkiksi graafinen käyttöliittymä, mutta komentoriviargumentit ovat edelleen kätevä tapa hoitaa yksinkertaisia tehtäviä.

Kotlinissa komentoriviargumentit ovat saatavilla `args` muuttujan kautta `main` funktion parametreina. Tämä muuttuja on taulukko, joka sisältää kaikki komentoriviargumentit, joita ohjelmaan on annettu.

## Katso myös:

- [Kotlin Doc - Command Line Arguments](https://kotlinlang.org/docs/tutorials/command-line.html)
- [Baeldung - Command Line Arguments in Kotlin](https://www.baeldung.com/kotlin/command-line-arguments)