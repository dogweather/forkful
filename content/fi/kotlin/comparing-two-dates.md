---
title:                "Kotlin: Vertailemalla kahden päivämäärän välillä"
programming_language: "Kotlin"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi
Miksi haluaisit vertailla kahta päivämäärää Kotlinin avulla? Usein esimerkiksi sovelluksissa tarvitaan tietoa siitä, kumpi päivämäärä on aiempi tai myöhempi, jotta voidaan tehdä tarkempia laskelmia tai näyttää käyttäjälle oikeat tiedot.

## Kuinka tehdä
Vertailu kahta päivämäärää Kotlinilla on helppoa ja nopeaa. Voit käyttää `LocalDate`-luokkaa ja sen `compareTo()`-metodia. Tässä esimerkissä vertailemme kahta päivämäärää ja tulostamme konsoliin tiedon siitä, kumpi päivämäärä on aiempi:
```Kotlin
// Luodaan kaksi LocalDate-oliota
val date1 = LocalDate.of(2020, 5, 15)
val date2 = LocalDate.of(2020, 6, 1)

// Vertaillaan päivämääriä
val result = date1.compareTo(date2)

// Tulostetaan tulos
if (result < 0) {
    println("$date1 on aiempi kuin $date2")
} else if (result > 0) {
    println("$date2 on aiempi kuin $date1")
} else {
    println("Päivämäärät ovat samat")
}
```
Tulostus:
```
2020-05-15 on aiempi kuin 2020-06-01
```

## Syvempi sukellus
Kotlinissa päivämäärän vertaileminen perustuu päivämäärän sisältävän `LocalDate`-olion `compareTo()`-metodiin. Tämä metodi palauttaa arvon, joka kertoo, kumpi päivämäärä on aiempi:
- Jos palautettu arvo on negatiivinen, ensimmäinen päivämäärä on aiempi kuin toinen.
- Jos palautettu arvo on positiivinen, toinen päivämäärä on aiempi kuin ensimmäinen.
- Jos palautettu arvo on nolla, päivämäärät ovat samat.

Voit myös käyttää `isBefore()` ja `isAfter()` -metodeita, jotka palauttavat boolean-arvon päivämäärien järjestyksestä. Lisää tietoa päivämäärien vertailusta löytyy Kotlinin virallisista dokumentaatioista.

## Katso myös
- [Kotlinin viralliset dokumentaatiot - Päivämäärät ja ajat](https://kotlinlang.org/docs/reference/dates.html)
- [Kotlinin LocalDate-luokka](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)