---
title:                "Nykyisen päivämäärän saaminen"
html_title:           "Java: Nykyisen päivämäärän saaminen"
simple_title:         "Nykyisen päivämäärän saaminen"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi

Miksi joku haluaisi käyttää Java-ohjelmointikieltä nykyisessä versiossaan datumin saamiseen?

Nykyään päivämäärän ja ajan seuraaminen on tärkeää monissa sovelluksissa ja ohjelmistoissa. Java tarjoaa helpon ja tehokkaan tavan hakea tämänhetkinen päivämäärä ja aika, jota voidaan käyttää esimerkiksi ajan leimaamiseen tai tietokantakyselyihin.

## Miten

Seuraavassa on esimerkkejä Java-koodista, joka osoittaa, miten voit hakea nykyisen päivämäärän ja ajan.

```Java
// Importoidaan java.util-paketti, jota tarvitaan nykyisen päivämäärän ja ajan saamiseen
import java.util.*;

// Luo Date-olio, joka sisältää nykyisen päivämäärän ja ajan
Date tänään = new Date();
// Tulostaa päivämäärän ja ajan muodossa "Elokuu 27 2021 13:30:00"
System.out.println(tänään);

// Luo Calendar-olio, joka sisältää nykyisen päivämäärän ja ajan
Calendar kalenteri = Calendar.getInstance();
// Hakee nykyisen vuoden, kuukauden, päivän ja ajan
int vuosi = kalenteri.get(Calendar.YEAR);
int kuukausi = kalenteri.get(Calendar.MONTH) + 1; // Huomaa, että kuukaudet alkavat 0:sta
int päivä = kalenteri.get(Calendar.DAY_OF_MONTH);
int tunti = kalenteri.get(Calendar.HOUR_OF_DAY);
int minuutti = kalenteri.get(Calendar.MINUTE);
int sekunti = kalenteri.get(Calendar.SECOND);
// Tulostaa päivämäärän ja ajan muodossa "08/27/2021 13:30:00"
System.out.printf("%02d/%02d/%d %02d:%02d:%02d", kuukausi, päivä, vuosi, tunti, minuutti, sekunti);
```

Esimerkkien suoritustulokset ovat seuraavat:

```
Fri Aug 27 13:30:00 EEST 2021
08/27/2021 13:30:00
```

## Syväsukellus

Java tarjoaa useita tapoja hakea nykyinen päivämäärä ja aika. Edellä esitetyt esimerkit käyttävät Date- ja Calendar-luokkia, mutta on myös muita vaihtoehtoja, kuten DateTimeFormatter-luokka ja LocalTime-luokka. Jokaisella näistä on omat etunsa, joten on tärkeää tutustua kuhunkin tarkemmin ja valita tarpeisiisi sopiva vaihtoehto.

## Katso myös

- [Oracle Java -dokumentaatio: Date](https://docs.oracle.com/en/java/javase/16/docs/api/java.base/java/util/Date.html)
- [Oracle Java -dokumentaatio: Calendar](https://docs.oracle.com/en/java/javase/16/docs/api/java.base/java/util/Calendar.html)
- [Oracle Java -dokumentaatio: DateTimeFormatter](https://docs.oracle.com/en/java/javase/16/docs/api/java.base/java/time/format/DateTimeFormatter.html)
- [Oracle Java -dokumentaatio: LocalTime](https://docs.oracle.com/en/java/javase/16/docs/api/java.base/java/time/LocalTime.html)