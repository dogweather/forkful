---
title:                "Kotlin: Päivämäärän muuntaminen merkkijonoksi"
simple_title:         "Päivämäärän muuntaminen merkkijonoksi"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Java-ohjelmointikielessä päivämäärän muuntaminen merkkijonoksi voi olla hankalaa ja vaivalloista. Onneksi Kotlin tarjoaa helpon ja tehokkaan tavan tehdä tämä prosessi sujuvaksi.

## Kuinka tehdä

### Tapa 1 - `java.text.DateFormat`

Kotlinissa voimme käyttää Java-luokkaa `java.text.DateFormat` päivämäärän muuntamiseen merkkijonoksi. Ensiksi, importtaamme luokan:

```Kotlin
import java.text.DateFormat
```

Sitten, luomme `DateFormat` -olion halutulla alueellamme ja asetamme päivämäärämuotoilijan haluamaksemme. Esimerkiksi, jos haluamme päivämäärän muotoon "dd.MM.yyyy", käytämme seuraavaa koodia:

```Kotlin
val dateFormat = DateFormat.getDateInstance(DateFormat.SHORT, Locale("fi", "FI"))
dateFormat!!.applyPattern("dd.MM.yyyy")
```

Viimeiseksi, käytämme `format` -metodia `dateFormat` -oliossamme muuntamaan päivämäärän merkkijonoksi:

```Kotlin
val date = Date()
val dateAsString = dateFormat.format(date)
println(dateAsString) // tulostaa esimerkiksi "24.03.2019"
```

### Tapa 2 - `java.time.format.DateTimeFormatter`

Toinen tapa muuntaa päivämäärä merkkijonoksi on käyttämällä Java 8 `java.time` -pakettia ja sen luokkaa `java.time.format.DateTimeFormatter`. Tämä tapa on joustavampi ja tarjoaa erilaisia vaihtoehtoja päivämäärän muotoiluun. Esimerkiksi, voimme muuttaa päivämäärän muotoon "EEEE, d. MMMM yyyy", jossa näytetään myös viikonpäivä:

```Kotlin
import java.time.LocalDate
import java.time.format.DateTimeFormatter

val formatter = DateTimeFormatter.ofPattern("EEEE, d. MMMM yyyy", Locale("fi", "FI"))
val formattedDate = LocalDate.now().format(formatter)
println(formattedDate) // tulostaa esimerkiksi "sunnuntai, 24. maaliskuuta 2019"
```

## Syväsukellus

Kotlin tarjoaa myös omia päivämäärän muotoilutyökaluita kuten `kotlinx-datetime`, jolla voi käsitellä päivämääriä ja aikoja helpommin ja tehokkaammin. Tämä kirjasto on kuitenkin tarkoitettu enemmän Android-sovelluksille kuin yleiseen ohjelmointiin.

## Katso myös

- [Kotlinin dokumentaatio päivämäärän muotoilusta](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/-date-time/)
- [Java 8 Java-ajan ja päivämäärän api](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)