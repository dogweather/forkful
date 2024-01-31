---
title:                "Merkkijonosta päivämäärän jäsentäminen"
date:                  2024-01-20T15:38:47.048734-07:00
html_title:           "Bash: Merkkijonosta päivämäärän jäsentäminen"
simple_title:         "Merkkijonosta päivämäärän jäsentäminen"

category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? - Mikä & Miksi?
Datasta string-muodossa päivämäärän irrottaminen tarkoittaa merkkijonossa olevan päivämäärän muuttamista päivämäärätyypiksi. Ohjelmoijat tekevät tätä, koska käyttäjän syötteet ja tiedostomuodot ovat usein tekstimuotoisia ja niitä on helpompi käsitellä ja tallentaa standardimuodossa.

## How to: - Näin teet:
```Swift
import Foundation

// Luodaan DateFormatter
let dateFormatter = DateFormatter()

// Määritellään päivämäärän formaatti
dateFormatter.dateFormat = "dd.MM.yyyy"

// Esimerkki stringistä joka sisältää päivämäärän
let dateString = "24.03.2023"

// Muunnetaan string päivämääräksi
if let parsedDate = dateFormatter.date(from: dateString) {
    print("Päivämäärä on: \(parsedDate)")
} else {
    print("Stringiä ei pystytty muuntamaan päivämääräksi.")
}
```

Sample output:
```
Päivämäärä on: 2023-03-24 00:00:00 +0000
```

## Deep Dive - Syvä sukellus
Ennen `DateFormatter`-luokkaa Swiftissä käytettiin muita keinoja, kuten C-kielen funktioita, päivämäärän parsimiseksi. `DateFormatter` tarjoaa kattavat työkalut päivämäärän formaatoinnille ja lokalisoinnille. Toinen vaihtoehto on käyttää `ISO8601DateFormatter`-luokkaa ISO 8601 -standardin mukaisille päivämäärille. Kun puhutaan implementaatiosta, on tärkeä muistaa, että `DateFormatter` on raskas luokka. Älä siis luo uutta instanssia joka kerta kun sitä tarvitset, vaan käytä olemassa olevia tai tallenna ne uudelleenkäytettäväksi. Lisäksi käyttäjän laitteen asetukset voivat vaikuttaa siihen, miten päivämääräformaatti tulisi määrittää.

## See Also - Katso myös
- [DateFormatter Class Reference](https://developer.apple.com/documentation/foundation/dateformatter)
- [Working with Date and Time in Swift](https://www.raywenderlich.com/5817-working-with-date-and-time-in-swift)
