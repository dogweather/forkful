---
title:                "Tulostaminen virheenjäljitystä varten"
html_title:           "Kotlin: Tulostaminen virheenjäljitystä varten"
simple_title:         "Tulostaminen virheenjäljitystä varten"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## Mikä ja miksi?
Debug-tulostamisella tarkoitetaan ohjelmakoodin suorituksen aikaisen tiedon tai virheiden näyttämistä. Ohjelmoijat käyttävät tätä työkaluna löytääkseen ja korjatakseen ohjelmiensa virheitä.

## Kuinka:
```Kotlin
// Esimerkki luokasta, jossa käytetään debug-tulostamista
class Henkilö(var nimi: String, var ikä: Int) {
    init {
        println("Uusi henkilö lisätty: $nimi, $ikä vuotta")
    }
    
    // Debug-tulostaminen funktiossa
    fun tervehdi() {
        println("Hei, olen $nimi ja olen $ikä vuotta vanha.")
    }
}

fun main(args: Array<String>) {
    val henkilö = Henkilö("Matti", 29)
    henkilö.tervehdi()
}

// Tulostaa:
// Uusi henkilö lisätty: Matti, 29 vuotta
// Hei, olen Matti ja olen 29 vuotta vanha.
```

## Syvällinen sukellus:
Debug-tulostamisen käsite on ollut käytössä jo pitkään ohjelmoinnin historiassa. Nykyään sen rinnalle on tullut muitakin työkaluja, kuten erilaiset debuggerit, jotka auttavat paikantamaan ja korjaamaan virheitä. Debug-tulostamisen toteutus vaihtelee eri kielten välillä, mutta yleisesti sitä käytetään lisäämällä ohjelman suoritusvaiheessa ```println()``` tai vastaava funktio haluttuihin kohtiin.

## Katso myös:
- [Debug-tulostaminen – Wikipedia](https://fi.wikipedia.org/wiki/Debug-tulostaminen)
- [Kotlin Debugging – Tutorialspoint](https://www.tutorialspoint.com/kotlin/kotlin_debugging.htm)
- [Debugging in Kotlin – Medium](https://medium.com/@duncan_irving/debugging-in-kotlin-c17e6cf6d314)