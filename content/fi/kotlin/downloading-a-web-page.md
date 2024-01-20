---
title:                "Verkkosivun lataaminen"
html_title:           "C#: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Verkkosivun lataamisella tarkoitetaan sen tietosisällön hakemista internetistä. Ohjelmoijat lataavat verkkosivuja päästäkseen käsiksi sen dataan ja käsittelemään sitä.

## Näin teet: 
```Kotlin
import java.net.URL

fun main(args: Array<String>) {
    println(URL("https://www.example.com").readText())
}```

Tämä esimerkki näyttää kuinka ladataan verkkosivun teksti. Koodi luo URL-olion, joka osoittaa tiettyyn verkkosivuun, ja käyttää 'readText'-metodia hakeakseen verkkosivun sisällön. Tulosteessa näet sivun HTML-koodin.

## Syvä Sukellus
Verkkosivujen lataaminen juontaa juurensa WWW:n alkutaipaleelle, jolloin verkkosivujen sisältöä käytettiin pääasiassa staattisen tiedon esittämiseen. Nykypäivänä sen avulla voidaan esimerkiksi hakea API:en kautta ajantasaisia tietoja, tai analysoida ja helpottaa digitaalista tiedon keruuta.

Vaihtoehtoisesti, tausta- tai palvelinohjelma voisi tehdä saman työn, mutta Kotlinin käyttö on suosittu valinta monestakin syystä. Sen tiiviys, tehokkuus ja yhteensopivuus Java-pohjaisten ohjelmistojen kanssa tekevät siitä houkuttelevan vaihtoehdon.

Kotlinin sisäänrakennettu `readText`-metodi tekee verkkosivun lataamisesta helppoa, mutta monimutkaisimmissa sovelluksissa saatetaan tarvita lisäkirjastoja, kuten JSoup, joka mahdollistaa tarkemman HTML-koodin käsittelyn.

## Katso Myös
- [Kotlinin verkkosivu](https://kotlinlang.org/)
- [JSoup-dokumentaatio](https://jsoup.org/)
- [Kotlinin IO-opas](https://kotlinlang.org/docs/io-operations.html)

Hyödynnä näitä resursseja syventyäksesi aiheeseen tai selvittääksesi mahdollisesti eteen tulevia ongelmia.