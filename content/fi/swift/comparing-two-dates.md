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

## Miksi
On monia tilanteita, jolloin ohjelmassa on tarve verrata kahta päivämäärää tai tarkistaa, ovatko ne samat. Esimerkiksi sovelluksissa, jotka käsittelevät varauksia tai tapahtumia, on tärkeää pystyä vertailemaan päivämääriä ja hallitsemaan tiettyjen päivämäärien välisiä suhteita.

## Kuinka
Vertaillessa päivämääriä Swiftissä on tärkeää käyttää `Date` -oliota ja sen tarjoamia metodeja ja ominaisuuksia. Tässä on muutamia esimerkkejä erilaisista tilanteista ja kuinka päivämääriä voidaan vertailla niissä.

### Vertaa kahta päivämäärää
Käytä `compare` -metodia vertailemaan kahta päivämäärää. Metodi palauttaa `ComparisonResult` -enumeration, joka voi olla `.orderedAscending`, `.orderedSame` tai `.orderedDescending` riippuen siitä, ovatko käytetyt päivämäärät suurempi, sama vai pienempi.

```Swift
let date1 = Date()
let date2 = Date(timeIntervalSinceNow: 86400) // yksi päivä tulevaisuudessa
let result = date1.compare(date2)
print(result) // tulostaa "orderedAscending"
```

### Tarkista, ovatko kaksi päivämäärää samat
Tässä tapauksessa voit käyttää `==` -operaattoria vertailemaan kahta päivämäärää.

```Swift
let date1 = Date()
let date2 = Date(timeIntervalSinceReferenceDate: date1.timeIntervalSinceReferenceDate)
if date1 == date2 {
    print("Päivämäärät ovat samat")
} else {
    print("Päivämäärät ovat erilaiset")
}
```

### Vertaa päivämäärien välisiä suhteita
`Date` -luokka tarjoaa myös `compareHierarchy` -metodin, jonka avulla voit vertailla päivämääriä tarkemmin. Tämä metodi ottaa huomioon paitsi päivämäärien tasaheiton, myös tunnit, minuutit ja sekunnit.

```Swift
let date1 = Date()
let date2 = Date(timeIntervalSinceNow: 3600) // yksi tunti tulevaisuudessa
let result = date1.compareHierarchy(date2)
print(result) // tulostaa "orderedSame"
```

## Syvällinen sukellus
Päivämäärien vertailu Swiftissä perustuu `TimeInterval` -tyyppiin, joka on todellisuudessa `Double` -tyyppiä. `Date` -luokka tarjoaa vain käteviä metodeja ja operaattoreita, jotka hyödyntävät tätä taustalla olevaa tyyppiä. Tämä varmistaa, että päivämäärät voidaan vertailla tarkasti ja luotettavasti.

## Katso myös
- [Swiftin virallinen verkkosivu] (https://developer.apple.com/swift/)
- [Apple Developer Documentation: Date] (https://developer.apple.com/documentation/foundation/date)