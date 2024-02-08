---
title:                "Nykyisen päivämäärän hankkiminen"
aliases:
- fi/swift/getting-the-current-date.md
date:                  2024-02-03T19:11:14.499567-07:00
model:                 gpt-4-0125-preview
simple_title:         "Nykyisen päivämäärän hankkiminen"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä ja miksi?
Nykyisen päivämäärän saaminen Swiftille tarkoittaa `Date` luokan käyttämistä päästäksesi käsiksi päivämäärään ja aikaan, jolloin sovellusta ajetaan. Ohjelmoijien on tarpeen hakea nykyinen päivämäärä monista syistä, jotka vaihtelevat tapahtumien aikaleimaamisesta, päivämäärälaskentojen suorittamiseen, päivämäärien ja aikojen näyttämiseen käyttöliittymässä.

## Kuinka:
Swiftin `Foundation` kehys tarjoaa `Date` luokan, mikä tekee nykyisen päivämäärän ja ajan saamisesta suoraviivaista. Tässä on perusesimerkki siitä, kuinka saada nykyinen päivämäärä:

```swift
import Foundation

let currentDate = Date()
print(currentDate)
```

Tämä tulostaa jotain tällaista:

```
2023-04-12 07:46:23 +0000
```

Tulosteen muoto noudattaa ISO 8601 -standardia, käyttäen UTC-aikavyöhykettä. Saatat kuitenkin haluta muotoilla tämän päivämäärän näyttötarkoituksiin. Swiftin `DateFormatter` luokka tulee avuksi:

```swift
let formatter = DateFormatter()
formatter.dateStyle = .long
formatter.timeStyle = .medium
let formattedDate = formatter.string(from: currentDate)
print(formattedDate)
```

Esimerkkituloste voisi olla:

```
12. huhtikuuta 2023 klo 10.46.23
```

Huomaa, että tulosteen muoto vaihtelee laitteen lokaation mukaan, jolla koodia ajetaan.

Projekteille, jotka vaativat monimutkaisempaa päivämäärän käsittelyä, monet Swift-kehittäjät kääntyvät kolmannen osapuolen kirjastojen, kuten `SwiftDate`, puoleen. Tässä on miten saatat käyttää `SwiftDate`a saadaksesi nykyisen päivämäärän tietyssä aikavyöhykkeessä ja muodossa:

Ensiksi, lisää `SwiftDate` projektiisi käyttäen SPM:ää, CocoaPodsia, tai Carthagea. Sitten:

```swift
import SwiftDate

let rome = Region(calendar: .gregorian, zone: .europeRome, locale: .current)
let currentDateInRome = DateInRegion(Date(), region: rome)
print(currentDateInRome.toFormat("yyyy-MM-dd HH:mm:ss"))
```

Tämä voisi tulostaa:

```
2023-04-12 09:46:23
```

Käyttämällä `SwiftDate`a, voit helposti käsitellä päivämääriä ja aikoja eri aikavyöhykkeissä ja lokaatioissa, yksinkertaistaen monimutkaisia päivämäärän käsittelytehtäviä Swift-sovelluksissasi.
