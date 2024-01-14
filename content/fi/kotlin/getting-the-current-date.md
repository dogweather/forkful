---
title:                "Kotlin: Päivämäärän haku"
programming_language: "Kotlin"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi: Ota selvää nykyisestä päivämäärästä

Nykyisen päivämäärän hakeminen voi olla hyödyllistä monessa ohjelmoinnin tilanteessa. Se voi auttaa sinua esimerkiksi luomaan raportteja, seuraamaan projektien edistymistä tai näyttämään käyttäjälle viimeisen päivitysajan.

## Kuinka: Esimerkkejä koodista ja tulosteista

```Kotlin
// Hae nykyinen päivämäärä
val currentDate = Date()

// Tulosta nykyinen päivämäärä
println(currentDate)

// Hae nykyinen päivämäärä ja aika
val currentDateTime = LocalDateTime.now()

// Tulosta nykyinen päivämäärä ja aika
println(currentDateTime)
```

Tässä esimerkissä käytämme Kotlinin `Date`-luokkaa nykyisen päivämäärän hakemiseen ja tulostamiseen. Voit myös käyttää `LocalDateTime`-luokkaa, joka sisältää myös tiedon ajasta.

## Syvällinen sukellus

Voit myös muokata nykyistä päivämäärää lisäämällä tai vähentämällä päiviä, kuukausia, vuosia tai sekunteja. Tätä varten voit käyttää `Calendar`-luokkaa ja sen metodeja, kuten `add` ja `roll`.

```Kotlin
// Hae nykyinen päivämäärä
val currentDate = Date()

// Luo uusi Calendar-instanssi ja aseta siihen nykyinen päivämäärä
val calendar = Calendar.getInstance()
calendar.time = currentDate

// Lisää yksi päivä
calendar.add(Calendar.DATE, 1)

// Tulosta tuleva päivämäärä
println(calendar.time)
```

## Katso myös

- [Kotlinin virallinen Dokumentaatio](https://kotlinlang.org/docs/reference/datetime.html)
- [Java Calendar Dokumentaatio](https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html)
- [LocalDateTime Dokumentaatio](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDateTime.html)