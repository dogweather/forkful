---
title:                "Swift: Kahden päivämäärän vertailu"
simple_title:         "Kahden päivämäärän vertailu"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi vertailla kahden päivämäärän välillä?

Kahden päivämäärän vertaileminen voi olla hyödyllistä esimerkiksi silloin, kun haluat tarkistaa, ovatko kaksi tapahtumaa tapahtuneet samana päivänä tai järjestää tapahtumat aikajärjestykseen.

## Kuinka vertailla kahden päivämäärän välillä?

Voit käyttää Swiftin Date-luokkaa kahden päivämäärän vertailuun. Aluksi tarvitset kaksi päivämäärää, joita haluat verrata. Voit luoda päivämäärät esimerkiksi näin:

```
let date1 = Date() // tämän päivän päivämäärä
let date2 = Date(timeIntervalSinceNow: -86400) // eilinen päivämäärä
```

Voit nyt verrata päivämääriä käyttämällä vertailuoperaattoreita, kuten `==`, `>`, `<` ja `>=`. Esimerkiksi:

```
if date1 == date2 {
  print("Päivämäärät ovat samat")
} else if date1 > date2 {
  print("Date1 on uudempi kuin date2")
} else {
  print("Date1 on vanhempi kuin date2")
}
```

Output riippuu siitä, mikä päivämäärä luotiin ensin.

## Syvemmälle kahden päivämäärän vertailuun 

Kun luot kaksi päivämäärää, varsinkin jos niiden välillä on tarkka aikaero, saattaa olla vaikeaa tarkistaa, ovatko ne todella samana päivänä. Tämä johtuu siitä, että Date-olioilla on myös tarkka kellonaika, eivätkä ne välttämättä ole täysin samassa muodossa.

Voit helposti tarkistaa päivämäärien välisen tarkemman aikarajan käyttämällä `Calendar`-luokkaa ja sen `isDate(_: equalToDate: toGranularity: in: maximumRange:)`-metodia. Esimerkiksi:

```
let calendar = Calendar.current
if calendar.isDate(date1, equalTo: date2, toGranularity: .day) {
  print("Päivämäärät ovat samana päivänä")
}
```

Voit myös vaihtaa tarkistetun aikarajan esimerkiksi tunneiksi tai minuuteiksi muuttaen `toGranularity`-parametrin arvoa.

## Katso myös

- [Apple Developer Documentation: Date](https://developer.apple.com/documentation/foundation/date)
- [Apple Developer Documentation: Calendar](https://developer.apple.com/documentation/foundation/calendar)
- [Stack Overflow: Comparing two dates in Swift](https://stackoverflow.com/questions/40357086/comparing-two-dates-in-swift)