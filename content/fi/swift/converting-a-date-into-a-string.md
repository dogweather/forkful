---
title:                "Päivämäärän muuntaminen merkkijonoksi"
html_title:           "Swift: Päivämäärän muuntaminen merkkijonoksi"
simple_title:         "Päivämäärän muuntaminen merkkijonoksi"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi?

Jokaisessa ohjelmointikielessä on tiettyjä tehtäviä, jotka voivat vaikuttaa aluksi yksinkertaisilta, mutta jotka voivat osoittautua haastaviksi. Yksi tällainen tehtävä on päivämäärän muuntaminen merkkijonoksi. Tämä voi olla tarpeellista esimerkiksi tietojen tallentamisessa tai tulostamisessa, ja juuri tästä syystä on tärkeää tietää, kuinka tämä tehtävä suoritetaan.

## Kuinka tehdä se?

Yleisin tapa muuntaa päivämäärä merkkijonoksi Swiftissä on käyttää `DateFormatter` -luokkaa. Käytä ```Swift 
let formatter = DateFormatter()
formatter.dateFormat = "dd.MM.yyyy"
let date = Date()
let dateString = formatter.string(from: date)
print(dateString) // tulostaa esimerkiksi "24.08.2021"
```
Merkittävin osa tässä koodissa on `dateFormat` -ominaisuuden asettaminen haluttuun muotoon. On tärkeää varmistaa, että tämä muoto vastaa haluttua tulostetta. Muista myös tuoda `Foundation` -kirjasto käyttöön, tai koodisi ei suoritu onnistuneesti.

## Syväsukellus

`DateFormatter` -luokka tarjoaa erilaisia ​​vaihtoehtoja päivämäärän ja ajan muuttamiseksi halutuksi merkkijonoksi. Voit esimerkiksi lisätä `timeStyle` -ominaisuuden ja antaa sille arvoksi esimerkiksi `short` tai `long` saadaksesi lisätietoja muotoilusta. Lisäksi `dateFormat` -ominaisuudessa voit käyttää muita merkkejä kuten `EEE` näyttääksesi viikonpäivän lyhennettynä tai `zzz` näyttääksesi aikavyöhykkeen.

## Katso myös

- [Apple Developer - DateFormatter](https://developer.apple.com/documentation/foundation/nsdateformatter)
- [SwiftLint - DateFormatter check](https://realm.github.io/SwiftLint/date_formatter.html)
- [How to Format Dates in Swift with DateFormatter](https://www.techotopia.com/index.php/How_to_Format_Dates_in_Swift_with_DateFormatter)