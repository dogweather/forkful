---
title:                "Java: Päivämäärän muuntaminen merkkijonoksi"
programming_language: "Java"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi muuttaa päivämäärä merkkijonoksi?

Päivämäärän muuttaminen merkkijonoksi on tärkeä osa ohjelmoinnin maailmaa. Päivämäärän muuttaminen merkkijonoksi mahdollistaa tietojen tallentamisen ja näyttämisen halutussa muodossa. Se on myös tarpeellista esimerkiksi tietokoneohjelmien lokien seurannassa ja raportoinnissa.

## Miten?

```java
import java.util.Date;
import java.text.SimpleDateFormat;

public class DateToString {
  public static void main(String[] args) {
    // Luodaan uusi Date-olio
    Date date = new Date();
    
    // Muutetaan päivämäärä merkkijonoksi halutussa muodossa
    SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy");
    String dateAsString = sdf.format(date);
    
    // Tulostetaan muunnettu päivämäärä
    System.out.println("Päivämäärä merkkijonona: " + dateAsString);
  }
}
```

**Tuloste:**

```
Päivämäärä merkkijonona: 14/02/2020
```

## Syvempi sukellus

Päivämäärän muuttaminen merkkijonoksi voi joskus aiheuttaa haasteita, esimerkiksi jos halutaan käyttää erilaista aikaväliä tai kelloaikaa kuin oletusasetuksena. Tällöin on tärkeää ymmärtää, miten muotoiluja ja kuviomerkkejä käytetään SimpleDateFormatissa. Lisäksi kannattaa huomioida eri aikavyöhykkeet, sillä ne voivat vaikuttaa muunnettavan päivämäärän arvoon.

## Katso myös

- [Java Date and Time](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [SimpleDateFormat](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [Java Tutorials: Formatting](https://docs.oracle.com/javase/tutorial/i18n/format/dateFormat.html)