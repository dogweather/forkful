---
title:                "Assosiatiivisten taulukoiden käyttö"
aliases:
- fi/swift/using-associative-arrays.md
date:                  2024-01-30T19:13:19.807711-07:00
model:                 gpt-4-0125-preview
simple_title:         "Assosiatiivisten taulukoiden käyttö"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?

Assosiatiiviset taulukot, tunnetaan Swiftissä sanakirjoina, antavat sinun tallentaa ja hallita tietoja avain-arvo -pareina. Ohjelmoijat käyttävät niitä järjestämään tietoja tehokkaasti, mikä tekee arvojen käyttämisen ja manipuloinnin helpommaksi niiden uniikkien avainten perusteella.

## Kuinka:

Swift tekee assosiatiivisten taulukoiden käytöstä suoraviivaista. Tässä on miten voit julistaa, lisätä, poistaa ja käyttää kohteita Swiftin sanakirjassa:

```Swift
// Sanakirjan julistaminen
var fruitColors: [String: String] = ["Apple": "Red", "Banana": "Yellow"]

// Uuden kohteen lisääminen
fruitColors["Grape"] = "Purple"

// Arvon käyttäminen sen avaimen avulla
if let appleColor = fruitColors["Apple"] {
    print("Omena on \(appleColor).")  // Tuloste: Omena on Punainen.
} else {
    print("Väriä ei löytynyt.")
}

// Kohteen poistaminen
fruitColors["Banana"] = nil  // Tämä poistaa "Banana" sanakirjasta

// Iterointi kohteiden läpi
for (fruit, color) in fruitColors {
    print("\(fruit) on \(color).")
    // Tuloste:
    // Omena on Punainen.
    // Viinirypäle on Purppura.
}
```

Sanakirjat ovat uskomattoman monipuolisia, sallien sinun manipuloida ja käyttää tietoja dynaamisesti. Niiden järjestämättömällä luonteella ei ole vaikutusta tietojen noutamisen nopeuteen, mikä on merkittävä etu käsiteltäessä suuria tietomääriä.

## Syväsukellus

Swiftin toteutus sanakirjoina assosiatiivisille taulukoille juontaa juurensa niiden voimakkaasta kyvystä kartoittaa uniikkeja avaimia arvoihin. Historiallisesti ohjelmointikielet ovat toteuttaneet tämän konseptin eri nimillä kuten hajautustaulut tai mapit, viitaten niiden toiminnallisuuteen luoda "kartta" avainten ja arvojen välille.

Swiftissä, sanakirjat on optimoitu suorituskykyä varten, hyödyntäen hajautettavia avaimia tehokkaaseen tietojen noutoon. Tämä tarkoittaa, että `[Key: Value]` sanakirjan `Key` tyypin on noudatettava `Hashable` protokollaa, mikä pätee useimpiin Swiftin vakiotyyppeihin kuten `Int`, `String` ja `Double`.

Yksi huomioitava seikka on, että vaikka sanakirjat ovat erinomaisia parien yhdistämiseen, ne eivät säilytä järjestystä. Jos sinun tarvitsee ylläpitää elementtien järjestystä, saatat tutkia muita vaihtoehtoja kuten `Array` järjestettyjen elementtien sekvenssille tai räätälöityjä tietorakenteita, jotka yhdistävät sekä taulukoiden että sanakirjojen ominaisuuksia.

On myös merkillepantavaa, että Swift kehittyy jatkuvasti, samoin sen käsittely ja optimointi sanakirjoille. Siksi on tärkeää pysyä ajan tasalla viimeisimmän Swift-dokumentaation kanssa hyödyntääksesi sanakirjoja parhaiten, varmistaen että käytät tehokkaimpia ja ajantasaisimpia käytäntöjä.
