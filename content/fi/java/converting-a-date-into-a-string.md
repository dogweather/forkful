---
title:                "Päivämäärän muuntaminen merkkijonoksi"
html_title:           "Java: Päivämäärän muuntaminen merkkijonoksi"
simple_title:         "Päivämäärän muuntaminen merkkijonoksi"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

Mikä & Miksi?

Päivämäärän muuntaminen merkkijonoksi tarkoittaa päivämäärän esittämistä tekstimuodossa, esimerkiksi "1.1.2020". Tätä tehdään usein ohjelmoinnissa, jotta päivämäärän käsittely olisi helpompaa ja tarkempaa.

Kuinka tehdä se:

```
Java

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class DateToString {
    public static void main(String[] args) {
        // Luo LocalDate -objekti halutulla päivämäärällä
        LocalDate date = LocalDate.of(2020, 1, 1);
        
        // Luo DateTimeFormatter -objekti halutulla formaatilla
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd.MM.yyyy");
        
        // Muunna päivämäärä merkkijonoksi käyttäen DateTimeFormatter -objektia
        String dateString = date.format(formatter);
        
        // Tulosta tulos
        System.out.println(dateString); // output: 01.01.2020
    }
}
```

Syväsukellus:

Päivämäärän muuttaminen merkkijonoksi on yleinen tehtävä ohjelmoinnissa ja sitä varten on erilaisia vaihtoehtoja. Yksi yleisesti käytetty vaihtoehto on Java 8:n tarjoama LocalDate ja sen tarjoamat DateTimeFormatterit. Myös vanhemmissa Java-versioissa on erilaisia tapoja muuntaa päivämäärä merkkijonoksi, kuten SimpleDateFormat-luokka.

Katso myös:

https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html

https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html

https://www.baeldung.com/java-date-to-string