---
title:    "Swift: Kaavion mukaiset merkkien poistaminen"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Miksi

Joskus ohjelmointiprojekteissa voi ilmetä tarve poistaa tekstistä tietyn kuvion mukaisia merkkejä. Tämä voi olla esimerkiksi tietty merkkijono tai tietyn muodon numerot. Tämä toiminto voi helpottaa tekstin käsittelyä ja jäsentämistä. 

## Kuinka tehdä

Ilmaisun poistamiseen tarkoitetulla toiminnolla on monia eri vaihtoehtoja Swift-ohjelmointikielessä, mutta ehkä yksinkertaisin tapa on käyttää `replacingOccurrences(of:with:)` - metodia. Alla olevassa esimerkissä poistetaan merkit "a" ja "i" merkkijonosta "Tervetuloa":

```
Swift let welcomeMessage = "Tervetuloa" let result = welcomeMessage.replacingOccurrences(of: "a", with: "", options: String.CompareOptions.literal, range: nil) println(result) // Tulostaa "Tervetulo" ``` 
Tässä esimerkissä käytetään myös `options` -parametria, joka määrittää, että tekstissä esiintyviä merkkejä käsitellään kirjaimina eikä esimerkiksi muistuttamaan tiettyä sanamuotoa. 

## Syvemmälle tekstinkäsittelyyn

Tekstin käsittelyyn liittyy paljon erilaisia tekniikoita ja toimintoja, jotka voivat auttaa tekstin jäsentämistä ja muokkaamista. Näihin kuuluu esimerkiksi tavallisten ilmaisujen käyttäminen, joiden avulla voidaan tunnistaa ja korvata tiettyjä merkkijonoja. Lisäksi on olemassa myös monia tietorakenteita, kuten taulukoita ja listoja, jotka voi hyödyntää tekstin käsittelyssä. 

## Katso myös

- [Swift-ohjelmointikielen virallinen verkkosivusto](https://swift.org)
- [Hienosäätäminen tekstiä Swiftissä](https://www.hackingwithswift.com/articles/142/how-to-tweak-strings-in-swift-using-replacingoccurrencesof)