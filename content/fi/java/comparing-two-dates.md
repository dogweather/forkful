---
title:    "Java: Kahden päivämäärän vertailu"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi verrata kahden päivämäärän välillä?

On monia syitä, miksi saattaa haluta vertailla kahta päivämäärää Java-ohjelmassa. Yleisimpiä syitä ovat aikavälien laskeminen, päivämäärien järjestäminen tai tietyn päivämäärän löytäminen tiettyyn tehtävään. Tässä blogikirjoituksessa kerromme, miten voit helposti vertailla kahden päivämäärän välillä Java-ohjelmassa.

## Kuinka tehdä se?

Vertaamme päivämääriä Java-ohjelmassa käyttäen `LocalDate`-luokkaa, joka edustaa yhden päivämäärän tietoja ilman aikatietoja. Voit luoda uuden `LocalDate`-olion antamalla sille vuoden, kuukauden ja päivän tiedot. Esimerkiksi:

```Java
LocalDate ensimmainenPaiva = LocalDate.of(2021, 9, 1);
LocalDate toinenPaiva = LocalDate.of(2021, 9, 10);
```

Kun sinulla on kaksi `LocalDate`-oliota, voit käyttää niiden `compareTo()`-metodia vertailemaan niitä. Tämä metodi palauttaa positiivisen luvun, jos ensimmäinen päivämäärä tulee toisen jälkeen, negatiivisen luvun, jos toinen päivämäärä tulee ensimmäisen jälkeen, ja nollan, jos päivämäärät ovat samat. Esimerkiksi:

```Java
int vertailu = ensimmainenPaiva.compareTo(toinenPaiva);
// vertailu = -1, koska ensimmainenPaiva on ennen toinenPaiva
```

## Syvempää tietoa päivämäärien vertailusta

Jos haluat tarkistaa, ovatko kaksi päivämäärää samassa kuussa tai vuodessa, voit käyttää `get()`-metodia `LocalDate`-oliota. Tämä metodi palauttaa tietyn elementin, kuten päivän tai vuoden. Voit myös käyttää `isAfter()` tai `isBefore()`-metodeita verrataksesi päivämääriä toisiinsa.

Kun vertaat päivämääriä, muista myös, että `LocalDate`-luokkaan kuuluu myös `equals()`-metodi, joka vertaa päivämääriä tarkasti. Tämä tarkoittaa, että jos kaksi päivämäärää ovat samat, niiden `equals()`-metodi palauttaa `true`-arvon.

## Katso myös

- [LocalDate-luokan dokumentaatio](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Java päivämäärien vertailu](https://www.baeldung.com/java-compare-dates)

Kiitos lukemisesta! Toivottavasti tämä artikkeli auttoi sinua ymmärtämään paremmin päivämäärien vertailua Java-ohjelmassa. Tässä oli vain pintaraapaisu aiheeseen, joten jos haluat oppia lisää, suosittelemme tutustumaan lisämateriaaleihimme. Nähdään seuraavassa blogikirjoituksessa!

## Katso myös