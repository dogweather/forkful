---
title:                "Kahden päivämäärän vertailu"
html_title:           "Swift: Kahden päivämäärän vertailu"
simple_title:         "Kahden päivämäärän vertailu"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Mitä & Miksi? 

Päivämäärien vertailu tarkoittaa kahden päivämäärän välisen eron selvittämistä ja vertailemista. Tämä on usein tarpeellista ohjelmoinnissa, kun halutaan esimerkiksi laskea päivien määrä tietyn tapahtuman välillä tai tarkistaa, onko jokin päivämäärä tulevaisuudessa vai menneisyydessä.

## Miten:

Seuraavassa esimerkissä näytämme, miten kahden päivämäärän välisen eron voi laskea Swiftillä:

```Swift
let date1 = Date(timeIntervalSince1970: 1577836800) // 1.1.2020
let date2 = Date(timeIntervalSince1970: 1609372800) // 1.1.2021

let difference = Calendar.current.dateComponents([.day], from: date1, to: date2).day
print(difference) // 366
```

Ensimmäisessä rivissä luomme kaksi päivämäärää, date1 ja date2, joita me sitten vertailemme. Toisessa rivissä käytämme Calendar-luokkaa laskemaan päivien määrä date1:n ja date2:n välillä. Lopuksi, print-funktio tulostaa eron, joka tässä tapauksessa on 366 päivää.

## Syvemmälle:

Päivämäärien vertailu on tärkeä osa ohjelmointia, ja se on ollut osana koodaamista jo pitkään. Aiemmin sen toteuttaminen vaati paljon enemmän koodia ja laskutoimituksia, mutta nykyään Swift tarjoaa helppokäyttöisiä työkaluja tämän tehtävän suorittamiseen.

Jos haluat vertailla päivämääriä tarkempia aikatietoja, kuten tunteja, minuutteja ja sekunteja, voit käyttää myös muita DateComponents-muuttujia, kuten .hour, .minute ja .second.

## Katso myös:

Jos haluat saada lisätietoa päivämäärien vertailusta ja laskemisesta Swiftillä, voit tutustua seuraaviin lähteisiin:

- Swiftin virallinen dokumentaatio päivämäärien vertailusta: https://developer.apple.com/documentation/foundation/datecomparisonresult
- Swiftin virallinen dokumentaatio DateComponents-muuttujista: https://developer.apple.com/documentation/foundation/datecomponents
- Stack Overflow:n sivusto, jolla käydään läpi päivämäärien vertailua Swiftissä: https://stackoverflow.com/questions/24723431/swift-days-between-giving-two-nsdates