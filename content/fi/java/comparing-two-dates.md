---
title:                "Kahden päivämäärän vertaaminen"
html_title:           "Bash: Kahden päivämäärän vertaaminen"
simple_title:         "Kahden päivämäärän vertaaminen"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Vertailu java-päivämäärässä

## Mitä & Miksi?

Päivämäärien vertailu on tarkistusprosessi, jossa määritetään kahden päivämäärän suhde. Ohjelmoijat tekevät tämän esimerkiksi aikajärjestyksen, aikavälien tai päivämääräsarjoissa olevien puutteiden määrittämiseksi.

## Kuinka:

Tässä on yksinkertainen esimerkki päivämäärien vertailusta Java 17:ssä `java.time.LocalDate`-luokan ja sen `isBefore()`, `isAfter()` ja `isEqual()` -metodien avulla:

```Java
import java.time.LocalDate;

public class Main {
    public static void main(String[] args) {
        // Luo päivämäärät
        LocalDate date1 = LocalDate.of(2020, 12, 1);
        LocalDate date2 = LocalDate.of(2021, 12, 1);

        // Vertaile päivämääriä
        if(date1.isBefore(date2)){
            System.out.println(date1 + " tulee ennen " + date2);
        }

        else if(date1.isAfter(date2)){
            System.out.println(date1 + " tulee jälkeen " + date2);
        }

        else{
            System.out.println("Päivämäärät ovat yhtä suuret");
        }
    }
}
```
Tämä koodi tulostaa:
```
2020-12-01 tulee ennen 2021-12-01
```

## Syvällisempi tarkastelu

Päivämäärien vertailumekaniikat ovat olleet osa Javaa version 1.0.2 julkaisusta lähtien, ja ne olivat osa `java.util.Date`-luokkaa. Tätä mekanismia on kuitenkin kritisoitu monimutkaisuuden ja epäjohdonmukaisuuden vuoksi. Tämän takia versiossa Java 8 otettiin käyttöön `java.time`-paketti, joka tarjoaa intuitiivisemman ja tehokkaamman tavan käsitellä päivämääriä ja aikaa.

On olemassa myös muita tapoja päivämäärien vertailemiseksi, esimerkiksi `java.time.Period`-luokkaa käyttämällä, jos haluat tietää päivämäärien välisten päivien, kuukausien tai vuosien määrän.

Java päivämäärän vertailutoimintojen toteutus liittyy `compareTo()` -metodiin, joka perustuu `java.lang.Comparable` -rajapintaan.

## Katso myös

- `java.time.LocalDate`: https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/time/LocalDate.html
- `java.util.Date`: https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/Date.html
- `Comparable`: https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/Comparable.html
- `java.time.Period`: https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/time/Period.html