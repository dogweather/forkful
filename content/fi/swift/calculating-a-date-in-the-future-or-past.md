---
title:                "Tulevan tai menneen päivämäärän laskeminen"
date:                  2024-01-20T17:32:24.159803-07:00
model:                 gpt-4-1106-preview
simple_title:         "Tulevan tai menneen päivämäärän laskeminen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Laskemme tulevaisuuden tai menneisyyden päivämääriä ohjelmoinnissa usein. Syyt voivat olla käyttäjän muistuttaminen, ajan kulumisen seuranta, tai päivämäärien vertailu.

## How to: (Kuinka tehdä:)
```Swift
import Foundation

// Nykyinen päivämäärä
let today = Date()

// Lasketaan päivämäärä kaksi viikkoa tulevaisuudessa
var futureDateComponenets = DateComponents()
futureDateComponenets.day = 14
if let futureDate = Calendar.current.date(byAdding: futureDateComponenets, to: today) {
    print("Kahden viikon päästä: \(futureDate)")
}

// Lasketaan päivämäärä 30 päivää menneisyydessä
var pastDateComponents = DateComponents()
pastDateComponents.day = -30
if let pastDate = Calendar.current.date(byAdding: pastDateComponents, to: today) {
    print("30 päivää sitten: \(pastDate)")
}
```

Tämä yksinkertainen koodi tulostaa päivämäärän kaksi viikkoa tästä päivästä eteenpäin ja päivämäärän 30 päivää taaksepäin.

## Deep Dive (Syväsukellus)
Päivämäärien käsittely on ollut ohjelmoinnin peruskiviä alusta alkaen. `DateComponents` ja `Calendar` ovat Swift-standardikirjaston työkaluja, jotka yksinkertaistavat ajan laskentaa. Ennen Swiftiä, Objective-C käytti NSDate:a, joka oli vähemmän intuitiivinen. Vaihtoehtoina Swiftissä voisi käyttää myös kolmannen osapuolen kirjastoja, kuten DateTools tai Timepiece, jotka tarjoavat lisäyksiä ja helpotuksia. Mutta Swiftin `Date` ja `Calendar` yleensä riittävät ja ovat suositeltava tapa menneiden ja tulevien päivämäärien laskemiseen, eri aikavyöhykkeet ja kulttuuriset erityispiirteet huomioon ottaen.

## See Also (Katso Myös)
- Apple Swift date and time programming guide: [https://developer.apple.com/documentation/foundation/date](https://developer.apple.com/documentation/foundation/date)
- Ray Wenderlichin opas päivämäärän käsittelyyn Swiftissä: [https://www.raywenderlich.com/5817-background-modes-tutorial-getting-started](https://www.raywenderlich.com/5817-background-modes-tutorial-getting-started)
