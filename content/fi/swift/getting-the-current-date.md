---
title:                "Nykyisen päivämäärän hankkiminen"
date:                  2024-01-20T15:16:34.136006-07:00
html_title:           "Bash: Nykyisen päivämäärän hankkiminen"
simple_title:         "Nykyisen päivämäärän hankkiminen"

category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (Mikä & Miksi?)
Haluatko tietää tämän hetken päiväyksen koodissasi? Se on arkipäivää esimerkiksi lokitiedoissa ja aikaleimoissa. Käyttämällä Swiftiä saat nykyisen päivämäärän ja kellonajan helposti, mikä auttaa sinua tallentamaan tai vertailemaan ajankohtia.

## How to: (Kuinka tehdä:)
```Swift
import Foundation

let nyt = Date()
print(nyt)
```
Tämä tulostaa jotain tämän kaltaista (riippuu ajankohdasta):
```
2023-04-12 14:23:46 +0000
```

## Deep Dive (Sukellus syvemmälle)
Swiftissä `Date` luokka on yleiskäyttöinen tapa edustaa tiettyjä hetkiä ajassa. Se kirjaa ajan ohitukset koordinoituna yleisaikana (UTC). Historiallisesti ajankäsittely on ollut monimutkaista ohjelmoinnissa eri aikavyöhykkeiden ja formaattien takia. Swift simplifioi tätä prosessia mutta antaa myös välineet syvemmälle hallintaan, kuten `DateFormatter` ja `Calendar` apit, näille, jotka tarvitsevat lisäkontrollia.

Aikaisemmissa kielissä kuten Objective-C:ssä piti käyttää `NSDate`, joka on käytännössä sama kuin Swiftin `Date`.

Jos haluat muuttaa päivämäärän merkkijonoksi tietyssä muodossa, voit tehdä näin:
```Swift
let muotoilija = DateFormatter()
muotoilija.dateFormat = "dd.MM.yyyy HH:mm"
let paivamaaranMerkkijono = muotoilija.string(from: nyt)
print(paivamaaranMerkkijono)
```
Tämä voisi tulostaa:
```
12.04.2023 17:23
```
Huomaa, että esimerkki käyttää suomalaista päivämäärän esitysmuotoa.

## See Also (Katso myös)
- Swiftin virallinen dokumentaatio `Date`-lohkosta: [Date - Swift Standard Library | Apple Developer Documentation](https://developer.apple.com/documentation/foundation/date)
- Ajanhallinta Swiftissä: [Work with Dates and Times in Swift | raywenderlich.com](https://www.raywenderlich.com/101-an-introduction-to-datetime-in-swift)
