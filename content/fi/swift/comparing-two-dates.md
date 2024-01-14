---
title:    "Swift: Kahden päivämäärän vertailu"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi

Vertaamalla kahta päivämäärää voit helposti tarkastella ajanjaksoja ja löytää tiettyjen tapahtumien väliset erot.

## Miten

Date-luokassa on valmiina `compare(_ date: Date) -> ComparisonResult` -metodi, jolla voit verrata kahta päivämäärää. Se palauttaa `ComparisonResult` -enumeration, joka voi olla `.orderedAscending`, `.orderedSame` tai `.orderedDescending`.

```Swift
let date1 = Date()
let date2 = Date(timeIntervalSinceNow: -86400) //yksi päivä aikaisemmin
let result = date1.compare(date2)

switch result {
case .orderedAscending:
    print("Ensimmäinen päivämäärä on ennen toista.")
case .orderedSame:
    print("Päivämäärät ovat samat.")
case .orderedDescending:
    print("Ensimmäinen päivämäärä on jälkeen toisen.")
}
```

Tässä esimerkissä verrataan kahta päivämäärää, `date1` ja `date2`. Koska `date1` on luotu myöhemmin, `result` tulostaessa `.orderedDescending`.

## Syvällinen sukellus

Vertaillessa kahta päivämäärää, voi olla tärkeää ottaa huomioon myös aikavyöhykkeet ja päivänvalon säästöaika, sillä ne voivat vaikuttaa vertailun lopputulokseen. Voit käyttää `Calendar.current.timeZone` -attribuuttia tarkastellaksesi haluatko ottaa nämä asiat huomioon vertailussa.

## Katso myös 

- [Swiftin virallinen dokumentaatio Date-luokasta](https://developer.apple.com/documentation/foundation/date)
- [Kirjoittajan blogiartikkeli: Ajanjaksojen laskeminen Swiftissä](https://esimerkkiblogi.fi/swiftin-ajanjaksojen-laskeminen)