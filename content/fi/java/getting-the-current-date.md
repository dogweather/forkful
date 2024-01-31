---
title:                "Nykyisen päivämäärän hankkiminen"
date:                  2024-01-20T15:14:52.828407-07:00
html_title:           "Bash: Nykyisen päivämäärän hankkiminen"
simple_title:         "Nykyisen päivämäärän hankkiminen"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (Mikä ja Miksi?)
Päivämäärän haaliminen ohjelmallisesti Java-kielessä tarkoittaa nykyhetken päivämäärän ja ajan selvittämistä. Sitä tehdään esimerkiksi logitiedostojen aikaleimoja varten tai kun käyttäjille näytetään päivän dataa.

## How to: (Miten tehdään:)
```java
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

public class CurrentDateExample {
    public static void main(String[] args) {
        // Päivämäärä ilman kellonaikaa
        LocalDate date = LocalDate.now();
        System.out.println("Päivämäärä: " + date);

        // Päivämäärä kellonajalla
        LocalDateTime dateTime = LocalDateTime.now();
        System.out.println("Päivämäärä ja aika: " + dateTime);

        // Muotoiltu päivämäärä
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd.MM.yyyy HH:mm:ss");
        String formattedDateTime = dateTime.format(formatter);
        System.out.println("Muotoiltu päivämäärä ja aika: " + formattedDateTime);
    }
}
```
### Näyte ulostulosta:
```
Päivämäärä: 2023-04-05
Päivämäärä ja aika: 2023-04-05T15:24:33.648
Muotoiltu päivämäärä ja aika: 05.04.2023 15:24:33
```

## Deep Dive (Syväsukellus):
Päivämäärän hakeminen Javassa on selvempää `java.time` -pakettia käytettäessä, joka tuli osaksi standardia Java 8 -versiossa (maaliskuu 2014). Ennen Java 8:aa käytettiin `java.util.Date` luokkaa, joka oli sekava ja epäjohdonmukainen. `java.time` -paketti, joka on myös tunnettu nimellä JSR-310, tuo mukanaan helpompaa APIa ja selkeämmän mallin päivämäärän käsittelyyn. Huomattavaa on, että paketti tukee myös aikavyöhykkeitä ja tarkkoja aikaleimoja, jotka ovat välttämättömiä globaaleissa sovelluksissa.

Vaihtoehtoja `LocalDate` ja `LocalDateTime` ovat esimerkiksi `ZonedDateTime` aikavyöhykkeellisen ajan käsittelyyn ja `Instant` tarkan hetkellisen aikaleiman saamiseksi.

## See Also (Katso Myös):
- [Oracle Java Documentation for java.time Package](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Baeldung Guide on Java 8 Date and Time](https://www.baeldung.com/java-8-date-time-intro)
- [Oracle Tutorial – Date Time explaining how to use java.time](https://docs.oracle.com/javase/tutorial/datetime/)
