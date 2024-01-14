---
title:                "Swift: Mallia vastaavien merkkien poistaminen"
simple_title:         "Mallia vastaavien merkkien poistaminen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi?

Päivittäisessä Swift-ohjelmoinnissa saattaa joskus ilmetä tarve poistaa tietyn kuvion mukaisia merkkejä string-tekstistä. Tämä voi johtua esimerkiksi turhista välilyönneistä tai muista tarpeettomista merkeistä, jotka halutaan poistaa ja jättää jäljelle vain halutut merkit.

## Näin teet sen

Alla on esimerkkejä koodista, joka poistaa merkkejä matching-kuviota vastaavista stringeistä käyttäen Swift-kieltä. Vaihda vain haluamasi string muuttujan arvo koodin ajoa varten ja saat nähdä tuotoksen koodin ajon jälkeen.

```Swift
// Alustetaan string-muuttuja
var string: String = "Tervetuloa Swift-ohjelmoinnin maailmaan!"

// Poistetaan välilyönnit stringistä
string = string.replacingOccurrences(of: " ", with: "")

//Tulostetaan muuttujan arvo, josta välilyönnit on poistettu
print(string)
// Output: TervetuloaSwift-ohjelmoinninmaailmaan!
```

```Swift
// Alustetaan toinen string-muuttuja
var string2: String = "Swift on ihana ohjelmointikieli!"

// Poistetaan kaikki kirjaimet paitsi vokaalit
string2 = string2.replacingOccurrences(of: "[^aeiouy]", with: "", options: .regularExpression, range: nil)

// Tulostetaan muuttujan arvo, jossa on jäljellä vain vokaalit
print(string2)
// Output: ioo-ieoieiei
```

## Syvenny aiheeseen

Stringien muokkaaminen on tärkeä osa ohjelmointia, ja Swift tarjoaa helppoja tapoja poistaa merkkejä matching-kuvioiden perusteella. Voit myös käyttää muita parametreja, kuten "range", jolla voit tarkentaa alueen, jolta haluat poistaa merkkejä. Tämän avulla voit esimerkiksi poistaa merkkejä vain tietystä kohdasta stringistä.

## Katso myös

* [Swiftin virallinen dokumentaatio](https://docs.swift.org/swift-book/)
* [Täydellinen opas stringien muokkaamiseen Swiftissä](https://www.hackingwithswift.com/articles/181/how-to-use-regular-expressions-in-swift)