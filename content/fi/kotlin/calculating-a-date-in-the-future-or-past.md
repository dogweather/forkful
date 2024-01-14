---
title:    "Kotlin: Päivämäärän laskeminen tulevaisuuteen tai menneisyyteen"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Miksi

Päivämäärien laskeminen tulevaisuudessa tai menneisyydessä voi olla hyödyllistä monessa eri tilanteessa, kuten esimerkiksi varauksia tehdessä tai tapahtumien suunnittelussa.

## Miten

Päivämäärien laskeminen Kotlin-ohjelmointikielellä on helppoa ja nopeaa. Alla on esimerkki koodista ja sen tulosteesta:

```Kotlin
val tanaan = Calendar.getInstance()
val huomenna = Calendar.getInstance()
huomenna.add(Calendar.DAY_OF_YEAR, 1)

println("Tänään on ${tanaan.get(Calendar.DAY_OF_MONTH)}.${tanaan.get(Calendar.MONTH) + 1}.${tanaan.get(Calendar.YEAR)}")
println("Huomenna on ${huomenna.get(Calendar.DAY_OF_MONTH)}.${huomenna.get(Calendar.MONTH) + 1}.${huomenna.get(Calendar.YEAR)}")
```

Tuloste:

> Tänään on 7.4.2021
>
> Huomenna on 8.4.2021

Käytämme tässä esimerkissä Calendar-luokkaa, joka tarjoaa mahdollisuuden muokata ja hakea tietoja ajasta ja päivämääristä. Huomenna-päivämäärä lasketaan lisäämällä yksi päivä tänään-päivämäärään.

## Syvällinen syventyminen

Jos haluat tarkempaa ohjausta päivämäärien laskemisessa, voit käyttää DateUtils-luokkaa, joka sisältää käteviä työkaluja päivämäärien muokkaamiseen. Esimerkiksi jos haluat laskea tietyn päivämäärän tietyn määrän päiviä eteenpäin, voit käyttää seuraavaa koodia:

```Kotlin
val alkuPäivä = Date()
val loppuPäivä = DateUtils.addDays(alkuPäivä, 5)

println("Alku päivä: $alkuPäivä")
println("Loppu päivä: $loppuPäivä")
```

Tuloste:

> Alku päivä: Wed Apr 07 09:33:28 UTC 2021
>
> Loppu päivä: Mon Apr 12 09:33:28 UTC 2021

Voit myös muokata päivämääriä muilla tavoilla, kuten lisäämällä tai vähentämällä tunteja tai minuutteja. Lisätietoja DateUtils-luokan käytöstä löydät Kotlinin virallisilta verkkosivuilta.

## Katso myös

- [Kotlin viralliset verkkosivut](https://kotlinlang.org/)
- [Kotlin DateUtils-dokumentaatio](https://developer.android.com/reference/android/text/format/DateUtils)
- [Kotlin dokumentaatio Calendar-luokasta](https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html)