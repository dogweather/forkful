---
title:    "Swift: Päivämäärän muuntaminen merkkijonoksi"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Päivämäärän muuntaminen merkkijonoksi (string) voi olla hyödyllistä, jos esimerkiksi haluat näyttää tietyn päivämäärän käyttäjälle tai tallentaa sen tietokantaasi. Tässä blogikirjoituksessa käydään läpi, miten tämä onnistuu Swift-ohjelmoinnissa.

## Kuinka

```Swift
let date = Date() // Luo uuden Date-olion
let dateFormatter = DateFormatter() // Luo uuden DateFormatter-olion
dateFormatter.dateFormat = "dd.MM.yyyy" // Määritä haluttu päivämäärän muoto
let dateString = dateFormatter.string(from: date) // Muunna päivämäärä merkkijonoksi käyttäen aiemmin määriteltyä muotoa
print(dateString) // Tulostaa esimerkiksi "06.08.2020"
```

Päivämäärän muuntaminen merkkijonoksi onnistuu luomalla ensin uusi Date-olio ja sen jälkeen DateFormatter-olio, jolla määritellään haluttu päivämäärän muoto. Lopuksi Date-olio muunnetaan merkkijonoksi käyttäen DateFormatter-oliota ja haluttua muotoa. Näin saadaan haluttu päivämäärä esimerkiksi tulostettua konsoliin tai tallennettua tietokantaan.

## Syvemmälle

DateFormatter-olion dateFormat-attribuutilla voi määrittää päivämäärän muodon lisäksi myös aikavyöhykkeen, kellonajan ja paljon muuta. Lisätietoa saat DateFormatterin dokumentaatiosta.

## Katso myös

- [DateFormatter Class Reference](https://developer.apple.com/documentation/foundation/dateformatter)
- [Date Formatting Guide - Apple Developer](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/DataFormatting/Articles/dfDateFormatting10_4.html)