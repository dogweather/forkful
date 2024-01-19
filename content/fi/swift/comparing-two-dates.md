---
title:                "Kahden päivämäärän vertaaminen"
html_title:           "Bash: Kahden päivämäärän vertaaminen"
simple_title:         "Kahden päivämäärän vertaaminen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Vertailla kahta päiväystä Swift-ohjelmoinnissa

## Mitä & Miksi?

Vertailla kahta päiväystä tarkoittaa kahden erilaisen ajankohdan suhteen tutkimista. Tässä tapauksessa se on välttämätöntä, koska ohjelmoijat tarvitsevat usein määrittämään aikaeron kahden päivämäärän välillä, tai päättämään, kumpi kahdesta päivästä tapahtui ensin.

## Miten:

Vertaillaan kahta päiväystä Swift 5:lla.

```swift
import Foundation

let formatter = DateFormatter()
formatter.dateFormat = "yyyy/MM/dd"

let date1 = formatter.date(from: "2019/02/14")!
let date2 = formatter.date(from: "2018/12/25")!

if date1.compare(date2) == .orderedAscending {
    print("Date1 is earlier than date2")
} else if date1.compare(date2) == .orderedDescending {
    print("Date1 is later than date2")
} else {
    print("The two dates are the same")
}
```

Tämä tuottaa seuraavan tulosteen:

```
Date1 is later than date2
```

## Syvempi sukellus:

Päivämäärien vertaaminen on historiallisesti ollut haasteellista, koska kalenterisysteemit ovat vaihdelleet niin paljon. Swift 5 edustaa kuitenkin merkittävää edistystä päivämäärien vertailussa, etenkin sen `Date.compare(_:)` -metodin ansiosta.

Sinun on kuitenkin aina muistettava, että päivämäärän vertaaminen on monimutkaista ja voi liittyä aikavyöhykkeeseen, aikaan ja kalenterin eroihin. On myös olemassa vaihtoehtoisia tapoja, kuten `TimeInterval` tai `Calendar` -objektien käyttö.

## Katso myös:

* [Virallinen Apple Foundation Framework (Sis. Date-metodit)](https://developer.apple.com/documentation/foundation)
* [Swift 5 päivämäärä- ja aikatoiminnot](https://www.hackingwithswift.com/articles/141/8-powerful-swift-features-that-arent-in-the-books)
* [Vertaile päivämääriä Swiftissä](https://www.hackingwithswift.com/example-code/system/how-to-compare-dates)