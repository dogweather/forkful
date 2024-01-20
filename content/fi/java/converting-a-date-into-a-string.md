---
title:                "Päivämäärän muuttaminen merkkijonoksi"
html_title:           "Go: Päivämäärän muuttaminen merkkijonoksi"
simple_title:         "Päivämäärän muuttaminen merkkijonoksi"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Java-Ohjelmointi: Päivämäärän Muuttaminen Merkkijonoksi

## Mikä & Miksi?

Päivämäärän muuttaminen merkkijonoksi ("converting a date to a string") tarkoittaa päivämäärä-olion muuttamista merkkijonoksi (String). Tätä esimerkiksi voidaan käyttää päivämäärien tulostamiseen käyttäjille tai tallentamiseen tekstitiedostoissa.

## Miten:

Käytetään Java'ssa `SimpleDateFormat` -luokkaa tähän. Koodiesimerkki:

```Java
import java.text.SimpleDateFormat;
import java.util.Date;

public class Main {
    public static void main(String[] args) {
        Date date = new Date();
        SimpleDateFormat formatter = new SimpleDateFormat("dd-MM-yyyy");
        String strDate = formatter.format(date);
        System.out.println("Päivämäärä merkkijonoksi: " + strDate);
    }
}
```

Tuloste:

```Java
Päivämäärä merkkijonoksi: 30-10-2022
```

## Syvällisempi käynti

### Historiallinen konteksti
Alun perin Java 1.0:ssä päivämäärien kanssa työskentely oli hankalaa. Korjauksia ja parannuksia tehtiin Java 1.1:n `SimpleDateFormat` -luokalla.

### Vaihtoehdot
Java 8 toi mukanaan uuden `java.time` paketin, joka on helpompi ja turvallisempi. Esimerkiksi:

```Java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class Main {
    public static void main(String[] args) {
        LocalDate date = LocalDate.now();
        String strDate = date.format(DateTimeFormatter.ofPattern("dd-MM-yyyy"));
        System.out.println("Päivämäärä merkkijonoksi: " + strDate);
    }
}
```

### Toteutus yksityiskohdat
`SimpleDateFormat` käyttää erityisiä merkkejä pohjan määrittämiseen (esim. "dd" päiville). Se muuntaa ne sitten päivämääräolson osiksi.

## Katso myös:

Java-dokumentaatio kattavasti selittää `SimpleDateFormat` ja `java.time`: 
- [Oracle's SimpleDateFormat Documentation](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [Oracle's DateTimeFormatter Documentation](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)

Niille, jotka haluavat syvällisempää tietoa päivämäärä- ja aikakäsittelystä Javassa, suosittelemme:
- [Baeldung's Article on Java's Date and Time API](https://www.baeldung.com/java-8-date-time-intro)