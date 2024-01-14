---
title:    "Java: Nykyisen päivämäärän hankkiminen"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi

Miksi olisi tärkeää saada nykyinen päivämäärä ohjelmallisesti? Nykyään useimmat laitteet ja sovellukset käyttävät päiväys- ja aikatietoja moniin eri tarkoituksiin. Esimerkiksi tekstiviestien aikaleimat, tapahtumakalenterit ja dokumenttien hallinta perustuvat kaikki päiväykseen ja aikaan. Siksi on tärkeää ymmärtää, kuinka saada nykyinen päivämäärä ja aika omassa ohjelmassasi, jotta voit hallita näitä tietoja ja tehdä sovelluksestasi toimivamman ja tehokkaamman.

## Kuinka

Saadaksesi nykyisen päivämäärän Javassa, voit käyttää `java.util.Date` -luokkaa. Tämä luokka tarjoaa erilaisia ​​metodeja, jotka mahdollistavat nykyisen päivämäärän, ajan ja aikavyöhykkeen hakemisen. Esimerkiksi:

```java
Date nykyinenPaivamaara = new Date();

// tulosta nykyinen päivämäärä
System.out.println("Nykyinen päivämäärä: " + nykyinenPaivamaara);

// tulosta päivämäärä ja aika tiettyyn muotoon
SimpleDateFormat muotoinenPaivamaara = new SimpleDateFormat("dd.MM.yyyy HH:mm:ss");
String paivamaara = muotoinenPaivamaara.format(nykyinenPaivamaara);
System.out.println("Nykyinen päivämäärä tiettyyn muotoon: " + paivamaara);

// aseta eri aikavyöhyke
TimeZone toinenAikavyohyke = TimeZone.getTimeZone("UTC");
muotoinenPaivamaara.setTimeZone(toinenAikavyohyke);
System.out.println("Nykyinen päivämäärä toisessa aikavyöhykkeessä: " + muotoinenPaivamaara.format(nykyinenPaivamaara));
```

Tämän esimerkin tuloste voi olla esimerkiksi:

```
Nykyinen päivämäärä: Mon Jan 25 12:47:13 EET 2021
Nykyinen päivämäärä tiettyyn muotoon: 25.01.2021 12:47:13
Nykyinen päivämäärä toisessa aikavyöhykkeessä: 25.01.2021 10:47:13
```

## Syvällisempi katsaus

`java.util.Date` -luokka ei ole täysin tarkka ja se käyttää tietokoneesi aikavyöhykkeen asetuksia, mikä voi aiheuttaa haasteita aikojen ja päivämäärien tarkkojen vertailujen kanssa. Tästä syystä on suositeltavaa käyttää `java.time` -pakettia, joka sisältää uuden `LocalDateTime` -luokan. Tämän luokan avulla voit hallita paikallista aikaa tai jopa päiväykseen liittämätöntä aikaa.

```java
// nykyinen paikallinen aika
LocalDateTime nykyinenAika = LocalDateTime.now();
System.out.println("Nykyinen aika: " + nykyinenAika);

// nykyinen UTC-aika
LocalDateTime nykyinenUtcAika = LocalDateTime.now(ZoneOffset.UTC);
System.out.println("Nykyinen UTC-aika: " + nykyinenUtcAika);
```

`java.time` -paketti tarjoaa myös monia muita hyödyllisiä luokkia ja metodeja päivämäärien ja aikojen hallintaan. Lue lisää Java 8 -dokumentaatiosta löytääksesi parhaiten sopivan ratkaisun tarpeisiisi.

## Katso myös

- [Java 8 -dokumentaatio](https://docs.oracle.com/javase/8/docs/api/)
- [Java.time-paketin opas