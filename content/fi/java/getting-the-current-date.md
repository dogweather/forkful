---
title:                "Nykyisen päivämäärän hankkiminen"
html_title:           "Haskell: Nykyisen päivämäärän hankkiminen"
simple_title:         "Nykyisen päivämäärän hankkiminen"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Mitä ja Miksi?

Nykyisen päivämäärän hankkiminen  on prosessi, jossa saat nyt käynnissä oleva päivämäärä ja aika. Ohjelmoijat tekevät sen esimerkiksi aikaleimojen luomiseksi, ajanhallinnan toteuttamiseksi ja päivämääräriippuvaisten toimintojen suorittamiseksi.

## Kuinka:

Seuraavat ovat esimerkkejä siitä, kuinka hankkia nykyinen päivämäärä Javassa:

```Java
// Java 8:sta lähtien
import java.time.LocalDate;

public class TamaPaiva {
    public static void main(String[] args){
        LocalDate tamaPaiva = LocalDate.now(); 
        System.out.println("Tämän päivän päivämäärä on: " + tamaPaiva);
    }
}
```
Kun suoritat tämän koodin, saat tulostuksen:

```Java
Tämän päivän päivämäärä on: 2022-08-30
```
## Deep Dive

Ennen Java 8:aa nykyisen päivämäärän saaminen oli hieman mutkikkaampaa ja sisälsi `java.util.Date`- tai `java.util.Calendar`-olio. Uusi `java.time`-paketti on käyttäjäystävällisempi ja tarjoaa parempaa aikavyöhykehallintaa.

Seuraavat ovat vaihtoehtoisia tapoja saada nykyinen päivämäärä Javassa:

```Java
// Vaihtoehto 1: käyttää java.util.Date
import java.util.Date;

Date nykyinenPaivamaara = new Date();
System.out.println("Tämän päivän päivämäärä on: " + nykyinenPaivamaara);
```

```Java
// Vaihtoehto 2: käyttää java.util.Calendar
import java.util.Calendar;

Calendar kalenteri = Calendar.getInstance();
Date nykyinenPaivamaara = kalenteri.getTime();
System.out.println("Tämän päivän päivämäärä on: " + nykyinenPaivamaara);
```

## Katso Myös

Lisätietoja Javan päivämääristä ja ajoista löytyy seuraavista lähteistä:

1. [Oracle Java Docs: java.time package](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
2. [Tutorial Point: Java - Date & Time](https://www.tutorialspoint.com/java/java_date_time.htm)
3. [Baeldung: A Guide to Java's LocalDate](https://www.baeldung.com/java-8-date-time-intro)