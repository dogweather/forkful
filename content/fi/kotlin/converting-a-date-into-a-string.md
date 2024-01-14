---
title:    "Kotlin: Päivämäärän muuntaminen merkkijonoksi"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

# Miksi muuttaa päivämäärä merkkijonoksi?

Päivämäärän muuttaminen merkkijonoksi on yleinen ohjelmointitarve monilla ohjelmointikielillä, mukaan lukien Kotlin. Tässä blogikirjoituksessa kerromme miksi päivämäärän muuttaminen merkkijonoksi on tärkeää ja kuinka se voidaan tehdä käyttämällä Kotlin-kieltä.

## Miten tehdä se?

Kotlinilla on joitain valmiita toimintoja päivämäärien muuttamiseksi merkkijonoksi. Yksinkertaisin tapa muuttaa päivämäärä merkkijonoksi on käyttää `SimpleDateFormat` -luokkaa.

```Kotlin
val date = Date()
val sdf = SimpleDateFormat("dd-MM-yyyy")
val dateString = sdf.format(date)
println(dateString)
```
Tämä koodi luo uuden `SimpleDateFormat` -olion ja määrittää muodon, jossa haluamme päivämäärän olevan. Voit määrittää haluamasi muodon käyttämällä `yy`, `yyyy`, `MM`, `dd` ja muita vastaavia symboleja. Lopuksi, `format` -metodilla voit muuttaa päivämäärän haluamasi muodon mukaiseksi merkkijonoksi.

Ulostulon tulisi näyttää esimerkiksi tältä: `09-10-2021`.

Voit myös käyttää `DateTimeFormatter` -luokkaa, joka on saatavilla Kotlinin Java-kirjaston kautta.

```Kotlin
val date = LocalDate.now()
val dtf = DateTimeFormatter.ofPattern("dd-MM-yyyy")
val dateString = date.format(dtf)
println(dateString)
```

Tämä koodi luo uuden `DateTimeFormatter` -olion ja antaa sille halutun muodon, jota komennolla `ofPattern` ohjataan. Sitten voit käyttää `format` -metodia muuttaaksesi päivämäärän haluamasi muodon mukaiseksi merkkijonoksi.

Ulostulon tulisi näyttää samalta kuin ensimmäisessä esimerkissä.

## Syvempi sukellus

Jos haluat selventää tarkemmin miten päivämääriä käsitellään merkkijonoina, voit tutustua Java Date and Time API -dokumentaatioon. Sieltä löydät lisätietoa käytetyistä luokista, kuten `SimpleDateFormat` ja `DateTimeFormatter`, sekä muita vinkkejä päivämäärien muuttamiseksi merkkijonoiksi.

Lisäksi, jos käytät Kotlinia Android-sovelluskehityksessä, voit tutustua Androidin Date and Time API -ohjeisiin, jotka sisältävät myös hyödyllistä tietoa päivämäärien muuttamisesta merkkijonoiksi.

# Katso myös

- [Java Date and Time API -dokumentaatio](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Android Date and Time API -ohjeet](https://developer.android.com/reference/java/time/package-summary)