---
title:                "Tilapäistiedoston luominen"
html_title:           "Kotlin: Tilapäistiedoston luominen"
simple_title:         "Tilapäistiedoston luominen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Miksi

Luodaan tilapäinen tiedosto (temporary file) ylimääräisen tilan luomiseksi väliaikaiseksi käyttöön tai tiedon tallentamiseksi lyhyeksi ajaksi. 

## Miten tehdä

Tilapäisen tiedoston luominen Kotlinilla on helppoa ja nopeaa. Seuraavassa muutamia esimerkkejä koodista ja niiden tulostuksesta:

```Kotlin

// Luodaan tilapäinen tiedosto "tempFile.txt" hakemistoon "documents/":
val tempFile = File("documents/", "tempFile.txt")
tempFile.createNewFile()

// Tarkistetaan, onko tiedosto luotu:
if(tempFile.exists()){
    println("Tilapäinen tiedosto luotu hakemistoon documents/")
}

// Kirjoitetaan tekstiä tiedostoon:
tempFile.printWriter().use { out ->
    out.println("Tervetuloa luomaan tilapäistä tiedostoa Kotlinilla!")
}

// Luetaan tiedoston sisältö:
println(tempFile.readText())

// Poistetaan tiedosto:
tempFile.delete()

// Tarkistetaan, että tiedosto on poistettu:
if(!tempFile.exists()){
    println("Tilapäinen tiedosto poistettu!")
}

```

Tulostus:

> Tilapäinen tiedosto luotu hakemistoon documents/
> Tervetuloa luomaan tilapäistä tiedostoa Kotlinilla!
> Tilapäinen tiedosto poistettu!

## Syvemmälle aiheeseen

Tilapäisen tiedoston luominen on hyödyllistä, kun tarvitsemme väliaikaisesti tallentaa tai käsitellä tietoa ilman, että se pysyvästi tallentuu järjestelmään. Tällainen tiedosto voi esimerkiksi olla väliaikainen työskentelytiedosto, jota ei tarvita enää kun työ on tehty.

Kotlin tarjoaa mahdollisuuden luoda tilapäisiä tiedostoja File-luokan createTempFile-metodilla. Tämä metodi luo automaattisesti uniikin tiedostonimen, joten käyttäjän ei tarvitse huolehtia siitä, että luodaanko tiedosto oikeaan hakemistoon ja että se saa uniikin nimen. Tiedoston poisto suoritetaan kätevästi File-luokan delete-metodilla, jolloin käyttäjän ei tarvitse erikseen poistaa tiedostoa.

## Katso myös

- [Kotlinin virallinen dokumentaatio tilapäisten tiedostojen luomisesta](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/create-temp-file.html)
- [Tilapäisen tiedoston luominen Javan avulla](https://www.baeldung.com/java-temporary-file)
- [Tiedostojen käsittely Kotlinilla](https://www.baeldung.com/kotlin-file-handling)