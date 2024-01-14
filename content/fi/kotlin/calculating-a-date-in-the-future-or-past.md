---
title:                "Kotlin: Muutaman päivän laskeminen tulevaisuudessa tai menneisyydessä"
simple_title:         "Muutaman päivän laskeminen tulevaisuudessa tai menneisyydessä"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

Joskus on hyödyllistä pystyä laskemaan tulevaisuuden tai menneisyyden päivämääriä esimerkiksi projektien tai tapahtumien suunnittelun yhteydessä.

## Kuinka

```Kotlin
// Lisätään vuodessa olevia päiviä tietty määrä 
val tulevaPaivamaara = LocalDate.now().plusDays(30)
println(tulevaPaivamaara) //tulostaa 30 päivää nykyisestä päivästä eteenpäin
```

```Kotlin
// Vähennetään päiviä tietystä päivämäärästä
val menneisyysPaivamaara = LocalDate.of(2020, 1, 1).minusDays(10)
println(menneisyysPaivamaara) //tulostaa 10 päivää ennen 1.1.2020
```

## Syvempi sukellus

Kotlin tarjoaa hauskoja ja käteviä tapoja laskea päivämääriä eteen- ja taaksepäin. LocalDate-luokassa olevat `plusDays()` ja `minusDays()` metodit ovat vain yksi esimerkki tästä. Myös muita aikayksiköitä, kuten kuukausia ja vuosia, voi lisätä ja vähentää päivämääriin vastaavien metodien avulla. Pysyvämmän ja tietokantaystävällisemmän ratkaisun tarjoaa `java.time.Period`-luokka.

## Katso myös

- [Kotlinin virallinen dokumentaatio](https://kotlinlang.org/docs/home.html)
- [Java LocalDate-luokan dokumentaatio](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Java Period-luokan dokumentaatio](https://docs.oracle.com/javase/8/docs/api/java/time/Period.html)