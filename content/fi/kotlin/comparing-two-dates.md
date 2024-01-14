---
title:    "Kotlin: Päivämäärien vertailu"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi vertailla kahta päivämäärää?

Vertailemalla kahta päivämäärää voimme selvittää, onko toinen päivämäärä ennen tai jälkeen toista, tai ovatko ne sama päivämäärä. Tämä voi olla hyödyllistä esimerkiksi laskiessa vuosia, päiviä tai tunteja kahden päivämäärän välillä. Kotlinin avulla tämä prosessi on helppoa ja nopeaa.

## Miten vertailla kahden päivämäärän välillä Kotlinissa?

Kotlin tarjoaa kätevän `LocalDate` -luokan, jolla voimme luoda päivämääriä ja vertailla niitä toisiinsa. Alla on esimerkki kahden päivämäärän vertailusta ja niiden tulostamisesta:

```Kotlin
val firstDate = LocalDate.of(2020, 1, 1)
val secondDate = LocalDate.of(2020, 12, 31)

println("Ensimmäinen päivämäärä: $firstDate")
println("Toinen päivämäärä: $secondDate")

if (firstDate.isBefore(secondDate)) {
  println("$firstDate on ennen $secondDate")
} else if (firstDate.isAfter(secondDate)) {
  println("$firstDate on jälkeen $secondDate")
} else {
  println("Päivämäärät ovat samat")
}
```
Tulostus:
```
Ensimmäinen päivämäärä: 2020-01-01
Toinen päivämäärä: 2020-12-31
2020-01-01 on ennen 2020-12-31
```

Voimme myös tarkastella päivämäärien eroa käyttäen `ChronoUnit` -luokkaa ja sen `between()` -metodia:

```Kotlin
val difference = ChronoUnit.DAYS.between(firstDate, secondDate)
println("Päivämäärien ero on $difference päivää")
```
Tulostus:
```
Päivämäärien ero on 365 päivää
```

## Syvempi sukellus päivämäärien vertailuun

Kotlinin `LocalDate` -luokka tarjoaa myös muita hyödyllisiä metodeja päivämäärien vertailuun, kuten `isLeapYear()` tarkastamaan, onko vuosi karkausvuosi, tai `plus()` ja `minus()` lisäämään tai vähentämään päiviä tarvittaessa. Lisäksi Kotlinin laajennusfunktiot mahdollistavat oman vertailumetodin luomisen, jos tarpeen.

## Katso myös

- [Kotlinin LocalDate-dokumentaatio](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/-local-date/)
- [Java 8 Date and Time API-opas](https://www.baeldung.com/java-8-date-time-intro)