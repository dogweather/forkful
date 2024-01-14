---
title:    "Kotlin: Päivämäärän laskeminen tulevaisuuteen tai menneisyyteen"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

Monet ohjelman rakentajat ja kehittäjät saattavat törmätä tarpeeseen laskea tietty päivämäärä etukäteen tai taaksepäin. Tämä voi johtua esimerkiksi tiettyjen toimintojen ajanrajoituksista tai tarpeesta tietää tulevien tapahtumien ajankohta. Onneksi tämä ongelma voidaan ratkaista helposti käyttämällä Kotlin-ohjelmointikielen valmiita ominaisuuksia.

## Kuinka

Kotlin tarjoaa Date-luokan, jota voidaan käyttää päivämäärän ja ajan käsittelyyn. Se tarjoaa myös kätevän toiminnon, jolla voidaan helposti lisätä tai vähentää päiviä, kuukausia tai vuosia nykyisestä päivämäärästä. Katso seuraavassa koodiesimerkissä, kuinka tämä toimii:

```Kotlin
import java.time.LocalDate

fun main(args: Array<String>) {

  val tanaan = LocalDate.now() // nykyinen päivämäärä
  val huomenna = tanaan.plusDays(1) // lisätään yksi päivä
  val ensiViikolla = tanaan.plusWeeks(1) // lisätään yksi viikko
  val ensiVuonna = tanaan.plusYears(1) // lisätään yksi vuosi

  println("Tänään on $tanaan") // tulostaa "Tänään on 2021-07-27"
  println("Huomenna on $huomenna") // tulostaa "Huomenna on 2021-07-28"
  println("Ensi viikolla on $ensiViikolla") // tulostaa "Ensi viikolla on 2021-08-03"
  println("Ensi vuonna on $ensiVuonna") // tulostaa "Ensi vuonna on 2022-07-27"
}
```

Huomaa, että voit myös vähentää päiviä, viikkoja tai vuosia käyttämällä funktiota "minusDays", "minusWeeks" tai "minusYears".

## Syventävä tarkastelu

Kotlinin Date-luokan lisäksi voit myös käyttää Java API:n Date- ja Calendar-luokkia päivämäärän käsittelyyn. Näillä luokilla on laajempi valikoima toimintoja ja ne voivat olla hyödyllisiä monimutkaisempien aikaluokkien hallitsemiseksi, kuten aikavyöhykkeiden tai ajanjaksojen käsittelyyn.

Kannattaa myös tutustua Java 8:n LocalDate-luokkaan, joka tarjoaa samankaltaisia toimintoja kuin Kotlinin Date-luokka. Se on uudempi ja parantaa Javan vanhempia date- ja time-luokkia.

## Katso myös

- [Kotlindocs - Java API](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/java.util.-date/)
- [Oracle Docs - LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)