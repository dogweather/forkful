---
title:                "Päivämäärän jäsennys merkkijonosta"
aliases:
- fi/kotlin/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:14:33.369339-07:00
model:                 gpt-4-0125-preview
simple_title:         "Päivämäärän jäsennys merkkijonosta"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?
Päivämäärän jäsentäminen merkkijonosta tarkoittaa tekstistä Date-objektiksi muuntamista. Tämä toiminto on olennainen sovelluksille, jotka käsittelevät käyttäjien syöttämiä päivämääriä tai ulkoisista tietoaineistoista peräisin olevia päivämääriä, mahdollistaen niiden helpomman käsittelyn ja muotoilun tarpeen mukaan.

## Kuinka:
Kotlin tukee päivämäärän jäsentämistä `java.time`-paketin avulla, joka otettiin käyttöön Java 8:ssa. Tässä on yksinkertainen lähestymistapa käyttäen `LocalDateTime`-luokkaa ja tiettyä kaavaa:

```kotlin
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

fun parseDateFromString(dateString: String): LocalDateTime {
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
    return LocalDateTime.parse(dateString, formatter)
}

fun main() {
    val dateString = "2023-04-01 12:00:00"
    val date = parseDateFromString(dateString)
    println(date)  // Tuloste: 2023-04-01T12:00
}
```

Joustavuuden lisäämiseksi, tai käsiteltäessä päivämääriä ulkoisista lähteistä kuten APIeista, saatat käyttää myös kolmannen osapuolen kirjastoa, kuten Joda-Time (vaikka se on vähemmän yleistä nyt, kun `java.time` on robusti). Kuitenkin JDK:n tarjoaman modernin lähestymistavan noudattaminen on suositeltavaa useimmissa Kotlin-sovelluksissa.

Jotta voisit jäsentää päivämäärän Kotlinissa käyttämättä kolmansien osapuolten kirjastoja, voit myös hyödyntää `SimpleDateFormat`-luokkaa versioissa ennen Java 8:aa tai Androidin API-tasoilla, joilla ei ole `java.time`-tukea:

```kotlin
import java.text.SimpleDateFormat

fun parseDateUsingSimpleDateFormat(dateString: String): java.util.Date {
    val formatter = SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
    return formatter.parse(dateString)
}

fun main() {
    val dateString = "2023-04-01 12:00:00"
    val date = parseDateUsingSimpleDateFormat(dateString)
    println(date)  // Tuloste vaihtelee aikavyöhykkeesi mukaan, esim., lauantai huhtikuun 01 12:00:00 GMT 2023
}
```

Muista aina asettaa aikavyöhyke, kun käytät `SimpleDateFormat`-luokkaa, jotta välttyisit odottamattomilta eroilta jäsentämissä päivämäärissä.
