---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:51.519459-07:00
description: "Nykyisen p\xE4iv\xE4m\xE4\xE4r\xE4n hankkiminen Javassa on perustoiminto,\
  \ jonka avulla ohjelmoijat voivat k\xE4sitell\xE4 p\xE4iv\xE4m\xE4\xE4r\xE4objekteja\
  \ toiminnoissa, kuten lokitukset,\u2026"
lastmod: '2024-03-13T22:44:56.455743-06:00'
model: gpt-4-0125-preview
summary: "Nykyisen p\xE4iv\xE4m\xE4\xE4r\xE4n hankkiminen Javassa on perustoiminto,\
  \ jonka avulla ohjelmoijat voivat k\xE4sitell\xE4 p\xE4iv\xE4m\xE4\xE4r\xE4objekteja\
  \ toiminnoissa, kuten lokitukset, p\xE4iv\xE4m\xE4\xE4r\xE4laskelmat ja aikaan perustuvat\
  \ ehdot."
title: "Nykyisen p\xE4iv\xE4m\xE4\xE4r\xE4n hankkiminen"
weight: 29
---

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
