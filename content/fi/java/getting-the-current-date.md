---
title:                "Java: Saada nykyinen päivämäärä"
simple_title:         "Saada nykyinen päivämäärä"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi

Monet Java-ohjelmat vaativat tietoa tämänhetkisestä päivämäärästä, ja tämän saamiseen on useita tapoja. Käyttämällä Java-ohjelmointia voit helposti hakea tämänhetkisen päivämäärän ja käyttää sitä tarkoituksesi mukaisesti.

## Kuinka

Voit saada tämänhetkisen päivämäärän käyttämällä Date-luokkaa. Alla on yksinkertainen koodiesimerkki, joka tulostaa tämänhetkisen päivämäärän terminaaliin:

```Java
import java.util.Date;

public class DateExample{
    public static void main (String[] args){
        Date currentDate = new Date();
        System.out.println("Tänään on: " + currentDate);
    }
}
```

**Tuloste:**

```
Tänään on: Sat Apr 17 15:36:53 EEST 2021
```
Tässä esimerkissä luomme uuden Date-olion, joka sisältää tämänhetkisen päivämäärän ja ajan. Tulostamalla tämän olion sisällön voimme nähdä tämänhetkisen päivämäärän ja ajan tarkkoine tiedostoineen.

Voit myös käyttää SimpleDateFormat-luokkaa mukauttamaan päivämäärän ja ajan esitysmuotoa. Alla esimerkki, jossa käytämme päivämäärän esitysmuotona "dd/MM/yyyy":

```Java
import java.util.Date;
import java.text.SimpleDateFormat;

public class DateExample{
    public static void main (String[] args){
        Date currentDate = new Date();
        SimpleDateFormat format = new SimpleDateFormat("dd/MM/yyyy");
        String date = format.format(currentDate);
        System.out.println("Tänään on: " + date);
    }
}
```

**Tuloste:**

```
Tänään on: 17/04/2021
```

## Syvällisempi tarkastelu

Java tarjoaa myös muita vaihtoehtoja tämänhetkisen päivämäärän hankkimiseen, kuten Calendar- ja LocalDateTime-luokat. Näissä luokissa on enemmän toiminnallisuutta päivämäärän ja ajan käsittelyyn, joten voit valita sopivimman vaihtoehdon tarpeidesi mukaan.

Voit myös asettaa halutun aikavyöhykkeen ja kieliasetuksen päivämäärälle käyttämällä Locale- ja TimeZone-luokkia. Tästä on hyötyä, jos haluat hakea päivämäärän ja ajan sijainnin mukaan.

## Katso myös

- [Java Date and Time - Oracle Documentation](https://docs.oracle.com/javase/tutorial/datetime/overview/iso.html)
- [Understanding Java's Date and Time API](https://www.baeldung.com/java-8-date-time-intro)
- [Date and Time in Java](https://www.javatpoint.com/java-date)