---
title:                "Swift: Kahden päivämäärän vertailu"
programming_language: "Swift"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

### Miksi vertailla kahta päivämäärää Swift-ohjelmointikielessä?

Joskus ohjelmoinnissa tarvitaan tietoa siitä, ovatko kaksi päivämäärää samanlaisia tai onko toinen päivämäärä suurempi kuin toinen. Tämä voi olla tarpeen esimerkiksi kalenteritoiminnallisuuksien toteuttamiseen tai tietokantojen kyselyjen tekemiseen. Swift-kielen Date-rakenne tarjoaa kätevät työkalut tämän vertailun tekemiseen ja seuraavaksi käyn läpi miten se tehdään.

### Kuinka vertailla kahta päivämäärää Swift-kielen avulla?

Vertailu tapahtuu Date-rakenteen compare-metodilla, joka palauttaa ComparisonResult-enumin arvon. Tämän Enumin arvo voi olla .orderedAscending, .orderedDescending tai .orderedSame riippuen siitä, onko ensimmäinen päivämäärä suurempi, pienempi tai yhtäsuuri kuin toinen. Alla näet esimerkkejä vertailuista ja niiden tuloksista.

```Swift
let date1 = Date() // nykyinen päivämäärä
let date2 = Date(timeIntervalSinceNow: 3600) // nykyinen päivämäärä lisättynä yksi tunti

let result = date1.compare(date2) // vertaillaan päivämääriä
if result == .orderedAscending {
    print("Ensimmäinen päivämäärä on pienempi kuin toinen.")
} else if result == .orderedDescending {
    print("Ensimmäinen päivämäärä on suurempi kuin toinen.")
} else {
    print("Päivämäärät ovat yhtä suuret.")
}

// Tulostaa: Ensimmäinen päivämäärä on pienempi kuin toinen.
```

Toinen tapa vertailla päivämääriä on käyttää < ja > operaattoreita, jotka vertailevat päivämäärän aikaleimoja (timestamp). Tämä tapahtuu luomalla uusi DateInterval-objekti, joka sisältää kaksi päivämäärää ja vertailemalla sen arvoa halutulla tavalla.

```Swift
let date1 = Date()
let date2 = Date(timeIntervalSinceNow: 3600)

if date1 < date2 {
    print("Tämä tulostuu, koska ensimmäinen päivämäärä on pienempi.")
}

if date1 > date2 {
    print("Tätä ei tulostu, koska ensimmäinen päivämäärä on pienempi.")
}

// Tulostaa: Tämä tulostuu, koska ensimmäinen päivämäärä on pienempi.
```

### Syvempi sukellus päivämäärien vertailuun Swift-kielessä

Date-rakenne tarjoaa myös muita hyödyllisiä metodeja päivämäärien vertailuun, kuten isEqual(to:), isAfter(_:) ja isBefore(_:) jotka vertailevat päivämääräobjekteja toisiinsa. Tämä määritys perustuu päivämäärän alkuhetkeen, joten siltä osin, se tarkastelee vain vuosia, kuukausia ja päiviä ja jättää kellonajat ja muut tekijät huomiotta. Lisäksi Date-rakenteen kautta voi selvittää myös päivämäärien välisen eron eri yksiköissä, kuten päivissä, kuukausissa ja vuosissa.

#### Katkelma koodista:
```Swift
let date1 = Date()
let date2 = Date(timeIntervalSinceNow: 3600)

if date1.isEqual(to: date2) {
    print("Päivämäärät ovat samat.")
}

if date1.isBefore(date2) {
    print("Ensimmäinen päivämäärä on ennen toista.")
}

let difference = date2.timeIntervalSince(date1) // ero