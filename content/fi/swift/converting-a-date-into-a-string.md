---
title:                "Swift: Päivämäärän muuntaminen merkkijonoksi"
simple_title:         "Päivämäärän muuntaminen merkkijonoksi"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Monissa tilanteissa tarvitaan muuttaa päivämäärä merkkijonoksi Swift-ohjelmoinnissa. Tämä voi olla esimerkiksi tarpeen, kun halutaan näyttää päivämäärä käyttäjälle tai tallentaa päivämäärä tietokantaan. Tässä blogikirjoituksessa käymme läpi miten tämä voidaan tehdä kätevästi Swiftillä.

## Miten tehdä

Monet ohjelmoijat käyttävät DateFormatter-luokkaa muuttaessaan päivämäärän merkkijonoksi. Tämä luokka mahdollistaa päivämäärän muotoilun halutulla tavalla ja tarjoaa myös monia muita hyödyllisiä toimintoja, kuten päivämäärän muuntamisen toiseen aikavyöhykkeeseen tai kielelle.

```Swift
let date = Date() // Luo uuden päivämäärän
let dateFormatter = DateFormatter() // Luo uuden DateFormatter-instanssin
dateFormatter.dateStyle = .medium // Asettaa päivämäärän muotoilun tyylin
let dateString = dateFormatter.string(from: date) // Muuntaa päivämäärän merkkijonoksi
print(dateString) // Tulostaa esimerkiksi "22.4.2021"
```

## Syvempi sukellus

Päivämäärän muuttaminen merkkijonoksi ei ole aina yksinkertaista. Esimerkiksi eri kielillä on erilaiset tavat ilmaista päivämäärä. Tässä tapauksessa voit käyttää Locale-luokkaa, joka määrittää maan ja kulttuurin, jossa ohjelmaa käytetään. Tällöin päivämäärä muualta tuleva käyttäjä näkee päivämäärän omalla kielellään.

```Swift
let date = Date() // Luo uuden päivämäärän
let dateFormatter = DateFormatter() // Luo uuden DateFormatter-instanssin
dateFormatter.dateStyle = .medium // Asettaa päivämäärän muotoilun tyylin
dateFormatter.locale = Locale(identifier: "fi_FI") // Asettaa suomen kielen käyttöön
let dateString = dateFormatter.string(from: date) // Muuntaa päivämäärän merkkijonoksi
print(dateString) // Tulostaa esimerkiksi "22. huhtikuuta 2021"
```

## Katso myös

- [Apple Developer Documentation - DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)
- [Swift by Sundell - Formatting dates in Swift](https://www.swiftbysundell.com/tips/formatting-dates-in-swift/)
- [Swift Tips - Easy date formatting in Swift with NaturalLanguage](https://swifttips365.com/2018/01/10/easy-date-formatting-in-swift-with-naturallanguage/)