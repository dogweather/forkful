---
title:                "Nykyisen päivämäärän hankkiminen"
aliases:
- /fi/java/getting-the-current-date.md
date:                  2024-02-03T19:09:51.519459-07:00
model:                 gpt-4-0125-preview
simple_title:         "Nykyisen päivämäärän hankkiminen"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?
Nykyisen päivämäärän hankkiminen Javassa on perustoiminto, jonka avulla ohjelmoijat voivat käsitellä päivämääräobjekteja toiminnoissa, kuten lokitukset, päivämäärälaskelmat ja aikaan perustuvat ehdot. Se on elintärkeää sovelluksissa, joissa seuranta, aikataulutus ja ajallinen datan analyysi ovat olennaisia.

## Miten:
Java tarjoaa useita tapoja hankkia nykyinen päivämäärä käyttäen sekä vanhaa `java.util.Date` -luokkaa että uudempaa `java.time` -pakettia (julkaistu Javassa 8), joka on monipuolisempi ja intuitiivisempi.

### Käyttäen `java.time.LocalDate`
```java
import java.time.LocalDate;

public class CurrentDateExample {
    public static void main(String[] args) {
        LocalDate currentDate = LocalDate.now();
        System.out.println(currentDate); // Esimerkkituloste: 2023-04-01
    }
}
```
### Käyttäen `java.time.LocalDateTime`
```java
import java.time.LocalDateTime;

public class CurrentDateExample {
    public static void main(String[] args) {
        LocalDateTime currentDateTime = LocalDateTime.now();
        System.out.println(currentDateTime); // Esimerkkituloste: 2023-04-01T12:34:56.789
    }
}
```
### Käyttäen `java.util.Date` (Vanha)
```java
import java.util.Date;

public class CurrentDateExample {
    public static void main(String[] args) {
        Date currentDate = new Date();
        System.out.println(currentDate); // Esimerkkituloste: Lau Apr 01 12:34:56 BST 2023
    }
}
```
### Kolmannen osapuolen kirjaston käyttäminen: Joda-Time
Ennen Java 8:aa Joda-Time oli de facto -standardi Java-datassa ja -ajassa. Jos työskentelet vanhojen järjestelmien parissa tai sinulla on mieltymys Joda-Timeen, tässä on miten voit käyttää sitä nykyisen päivämäärän hankkimiseen:
```java
import org.joda.time.LocalDate;

public class CurrentDateExample {
    public static void main(String[] args) {
        LocalDate currentDate = LocalDate.now();
        System.out.println(currentDate); // Esimerkkituloste: 2023-04-01
    }
}
```
**Huom:** Vaikka `java.util.Date` ja Joda-Timea käytetään yhä, uusiin projekteihin suositellaan `java.time` -pakettia sen muuttumattomuuden ja kattavan API:n ansiosta päivämäärien ja aikojen käsittelyyn.
