---
title:                "Kotlin: Päivämäärän laskeminen tulevaisuudesta tai menneisyydestä"
programming_language: "Kotlin"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Miksi laskurilla olisi merkitystä ohjelmoinnissa?

Päivämäärien laskeminen tulevaisuuteen tai menneisyyteen voi olla hyödyllistä esimerkiksi sovelluksissa, jotka vaativat tarkan ajan määrittämistä. Se voi myös auttaa laskemaan tulevia tapahtumia tai käsittelemään aikaperusteista dataa.

## Kuinka laskea päivämääriä Kotlinilla

Jos haluat laskea päivämääriä tulevaisuuteen tai menneisyyteen Kotlinilla, voit käyttää `Calendar` -luokkaa ja sen `add` -metodia. Tässä on yksinkertainen esimerkki, joka laskee päivän päähän nykyisestä päivästä:

```Kotlin
val cal = Calendar.getInstance()
cal.add(Calendar.DAY_OF_YEAR, 1)
val tulevaPaiva = cal.time
println("Tuleva päivä on: $tulevaPaiva")
```

Tulostus olisi:

```
Tuleva päivä on: Tue Jul 14 17:47:52 EEST 2020
```

Voit myös määrittää tietyn päivämäärän, johon haluat lisätä tai vähentää päiviä. Tämä voidaan tehdä asettamalla `Calendar` -luokan `time` -ominaisuus halutuksi päivämääräksi. Esimerkiksi, jos haluat laskea päivämäärää 10 päivän päähän, voit käyttää seuraavaa koodia:

```Kotlin
val cal = Calendar.getInstance()
cal.time = Date(2020, 6, 14)
cal.add(Calendar.DAY_OF_YEAR, 10)
val tulevaPaiva = cal.time
println("Tuleva päivä on: $tulevaPaiva")
```

Tulostus olisi:

```
Tuleva päivä on: Fri Jun 24 00:00:00 EEST 2020
```

## Syventävä tarkastelu

Kzechma on avoimen lähdekoodin kirjasto, joka tarjoaa erilaisia työkaluja päivämäärien laskemiseen Kotlinissa. Se tarjoaa muun muassa metodeja pysyvien ja kausien päivämäärien laskentaan.

Voit myös muokata `Calendar` -luokkaan liittyviä asetuksia, kuten käyttää eri aikavyöhykkeitä tai asettaa tietyn päivämäärän ensimmäiseksi viikoksi. Tämä voi olla hyödyllistä, jos haluat laskenut päivämäärät noudattamaan tiettyä aikavyöhykettä tai viikkoesitystä.

## Katso myös

- [Kzechma-kirjasto](https://github.com/Kzechma/Kzechma)
- [Kotlindokumentaatio - Calendar-luokka](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-calendar/)
- [Kotlindokumentaatio - Date-luokka](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-date/)
- [Kotlindokumentaatio - Time-luokka](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-time/)