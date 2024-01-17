---
title:                "Kahden päivämäärän vertailu"
html_title:           "Kotlin: Kahden päivämäärän vertailu"
simple_title:         "Kahden päivämäärän vertailu"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Päivämäärien vertaaminen on yksinkertainen tapa selvittää, mikä päivämäärä on aikaisempi tai myöhempi toiseen verrattuna. Tämä on hyödyllistä esimerkiksi silloin, kun haluat tarkistaa, onko jokin tapahtuma jo tapahtunut vai odotatko sitä vielä. Ohjelmoijat käyttävät tätä toimintoa usein monimutkaisempien sovellusten luomiseen, jotka vaativat päivämäärien vertailua ja päivämääriin liittyvää logiikkaa.

## Näin teet sen:
### Yksinkertainen vertailu:
```Kotlin
val ensimmäinenPäivä = LocalDate.parse("2021-01-01")
val toinenPäivä = LocalDate.parse("2021-01-05")

if (ensimmäinenPäivä < toinenPäivä) {
  println("Ensimmäinen päivä on ennen toista päivää.")
} else if (ensimmäinenPäivä > toinenPäivä) {
  println("Ensimmäinen päivä on jälkeen toista päivää.")
} else {
  println("Päivämäärät ovat samat.")
}
```
Tulostus:
```
Ensimmäinen päivä on ennen toista päivää.
```

### Tarkempi vertailu:
```Kotlin
val ensimmäinenPäivä = LocalDate.parse("2021-01-01")
val toinenPäivä = LocalDate.parse("2021-01-05")

if (ensimmäinenPäivä.isBefore(toinenPäivä)) {
  println("Ensimmäinen päivä on ennen toista päivää.")
} else if (ensimmäinenPäivä.isAfter(toinenPäivä)) {
  println("Ensimmäinen päivä on jälkeen toista päivää.")
} else {
  println("Päivämäärä on sama.")
}
```
Tulostus:
```
Ensimmäinen päivä on ennen toista päivää.
```

## Syventävä tieto:
Ennen Java 8:aa päivämääriä vertailtiin käyttämällä `Date`-luokan `before()` ja `after()` metodeita. Nykyään Kotlinin `LocalDate`-luokka tarjoaa oman `isBefore()` ja `isAfter()` metodit päivämäärien vertailuun. Lisäksi jotkin kolmannen osapuolen kirjastot, kuten Joda-Time, tarjoavat kattavampia päivämäärätoimintoja.

## Katso myös:
- [Kotlinin LocalDate-dokumentaatio](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-local-date/)
- [Java 8:n LocalDate-dokumentaatio](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Joda-Time-kirjaston kotisivu](https://www.joda.org/joda-time/)