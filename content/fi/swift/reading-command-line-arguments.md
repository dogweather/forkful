---
title:                "Komentoriviparametrien lukeminen"
html_title:           "Swift: Komentoriviparametrien lukeminen"
simple_title:         "Komentoriviparametrien lukeminen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi

Jos haluat hallita ja hallita ohjelman käyttäytymistä ulkoisten tekijöiden avulla, kuten komentoriviparametrit, on tärkeää tietää, kuinka lukea nämä parametrit Swiftillä. Tässä artikkelissa opit kuinka voit helposti lukea komentoriviparametreja ja käyttää niitä ohjelman suorituksessa.

## Miten

Command line argumenttien lukeminen Swiftissä on yksinkertaista ja vaatii vain muutaman rivin koodia. Ensimmäiseksi luodaan `CommandLine` objekti, joka sisältää kaikki komentoriviparametrit. Sitten voit käyttää `CommandLine` objektin metodeja saadaksesi haluamasi parametrit.

```Swift
// Luodaan `CommandLine` objekti
let commandLine = CommandLine()

// Saadaan haluttu parametri
let parameter = commandLine.arguments[0]
```

Tämän jälkeen voit käyttää parametreja haluamallasi tavalla koodissasi. Esimerkiksi voit tarkistaa, onko tietty parametri annettu vai ei ja suorittaa tarvittavat toiminnot sen perusteella.

```Swift
// Tarkistetaan, onko parametri annettu
if commandLine.arguments.contains("-h") {
    // Suoritetaan tarvittavat toiminnot
    print("Tervetuloa ohjelman käyttöoppaaseen!")
}
```

## Syvempi sukellus

`CommandLine` objektissa on monia hyödyllisiä metodeja, joita voit käyttää komentoriviparametrien lukemiseen. Esimerkiksi voit käyttää `first` ja `last` metodeja saadaksesi ensimmäisen ja viimeisen komentoriviparametrin. Voit myös käyttää `namedArguments` metodia saadaksesi nimettyjä parametreja, joissa on `--` etuliite.

```Swift
// Saadaan ensimmäinen ja viimeinen parametri
let firstArgument = commandLine.arguments.first
let lastArgument = commandLine.arguments.last

// Saadaan nimetty parametri
let namedParameter = commandLine.namedArguments["--nimi"]
```

On myös huomionarvoista, että `CommandLine` objekti sisältää myös itse ohjelman nimen, jota voi käyttää tarvittaessa.

```Swift
// Saadaan ohjelman nimi
let programName = commandLine.programName
```

## Katso myös

Tässä artikkelissa opit lukemaan komentoriviparametreja Swiftissä, mutta tämän lisäksi on myös muita hyödyllisiä toimintoja, joita voit käyttää Swiftissä ohjelmointisi helpottamiseksi. Tässä muutamia linkkejä, joiden avulla voit tutustua lisää Swiftin ominaisuuksiin ja käyttötapoihin:

- [Viralliset Swift-dokumentaatiot](https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html)
- [Swiftin opetusohjelmat Ray Wenderlichillä](https://www.raywenderlich.com/5256505-swift-tutorial-for-beginners-part-1-getting-started)
- [Swiftin opetusohjelmat Hacking with Swiftissä](https://www.hackingwithswift.com/quick-start/swiftui)