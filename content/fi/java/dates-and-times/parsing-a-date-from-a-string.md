---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:33.339772-07:00
description: ''
lastmod: '2024-04-05T22:00:09.158626-06:00'
model: gpt-4-0125-preview
summary: ''
title: "P\xE4iv\xE4m\xE4\xE4r\xE4n j\xE4sennys merkkijonosta"
weight: 30
---

## Kuinka:


### Käyttäen `java.time` pakettia (Suositellaan Java 8:ssa ja myöhemmissä):
```java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class PaivamaaranJasentaja {
    public static void main(String[] args) {
        String paivamaaraMerkkijono = "2023-04-30";
        DateTimeFormatter muotoilija = DateTimeFormatter.ofPattern("yyyy-MM-dd");
        LocalDate paivamaara = LocalDate.parse(paivamaaraMerkkijono, muotoilija);
        System.out.println(paivamaara); // Tuloste: 2023-04-30
    }
}
```

### Käyttäen `SimpleDateFormat` (Vanhempi Lähestymistapa):
```java
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

public class PaivamaaranJasentaja {
    public static void main(String[] args) {
        String paivamaaraMerkkijono = "30/04/2023";
        SimpleDateFormat muotoilija = new SimpleDateFormat("dd/MM/yyyy");
        try {
            Date paivamaara = muotoilija.parse(paivamaaraMerkkijono);
            System.out.println(paivamaara); // Tulosteen muoto riippuu järjestelmäsi oletusmuodosta
        } catch (ParseException e) {
            e.printStackTrace();
        }
    }
}
```

### Käyttäen Kolmannen Osapuolen Kirjastoja (esim., Joda-Time):
Joda-Time on ollut merkittävä kolmannen osapuolen kirjasto, mutta on nyt ylläpitotilassa `java.time`-paketin käyttöönoton johdosta Java 8:ssa. Kuitenkin, niille, jotka käyttävät Java-versioita ennen 8:aa, Joda-Time on hyvä valinta.
```java
import org.joda.time.LocalDate;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;

public class PaivamaaranJasentaja {
    public static void main(String[] args) {
        String paivamaaraMerkkijono = "2023-04-30";
        DateTimeFormatter muotoilija = DateTimeFormat.forPattern("yyyy-MM-dd");
        LocalDate paivamaara = LocalDate.parse(paivamaaraMerkkijono, muotoilija);
        System.out.println(paivamaara); // Tuloste: 2023-04-30
    }
}
```
Huomaa, että työskennellessäsi päivämäärien kanssa, ole aina tietoinen aikavyöhykkeen asetuksista, jos jäsennät tai muotoilet päivämäärä-aikoja eikä vain päivämääriä.
