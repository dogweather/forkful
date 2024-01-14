---
title:    "Java: Kahden päivämäärän vertaaminen"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Miksi

Miksi sinun kannattaa vertailla kahta päivämäärää Java-ohjelmoinnissa? Päivämäärien vertaileminen on hyödyllistä esimerkiksi silloin, kun sinun täytyy tarkistaa, onko jokin tapahtuma tapahtunut tiettynä päivänä tai onko päivämäärä ennen tai jälkeen toista tiettyä päivämäärää.

## Miten

Vertailemalla kahta päivämäärää Java-ohjelmoinnissa voit käyttää DateTimeFormatter-luokkaa ja sen parse()-metodia, joka muuntaa merkkijonon päivämääräksi. Tämän jälkeen voit käyttää metodeja, kuten isAfter() ja isBefore() tarkistaaksesi, mikä päivämäärä on aikaisempi tai myöhempi. Voit myös käyttää compareTo()-metodia, joka palauttaa arvon 0, jos päivämäärät ovat samat, positiivisen luvun, jos ensimmäinen päivämäärä on myöhempi ja negatiivisen luvun, jos ensimmäinen päivämäärä on aikaisempi.

```Java
// Päivämäärät
LocalDate ensimmainenPvm = LocalDate.of(2021, 5, 24);
LocalDate toinenPvm = LocalDate.of(2021, 6, 15);

// Vertaillaan päivämääriä
boolean onEnnen = ensimmainenPvm.isBefore(toinenPvm);
boolean onJalkeen = ensimmainenPvm.isAfter(toinenPvm);

// Käytetään compareTo()-metodia
int tulos = ensimmainenPvm.compareTo(toinenPvm);
```

**Tulostus:**
```
onEnnen: true
onJalkeen: false
tulos: -1
```

## Syvempi sukellus

Päivämäärien vertaileminen voi olla hieman monimutkaisempaa, jos haluat tarkastella myös aikaa tai aikavyöhykettä. Tällöin voit käyttää LocalDateTime- ja ZonedDateTime-luokkia, jotka sisältävät myös ajan ja aikavyöhykkeen tiedot. Voit myös käyttää Duration- tai Period-luokkia, jotka auttavat laskemaan aikaa tai päiviä kahden päivämäärän välillä.

Esimerkiksi voit tarkistaa, onko jokin tapahtuma tapahtunut tiettynä aikavälinä tarkistamalla, onko päivämäärä LocalDateTime-oliolla välillä kahden ZonedDateTime-olion välillä.

```Java
// Aikavälit
LocalDateTime tapahtumaAika = LocalDateTime.of(2021, 6, 10, 15, 30);
ZonedDateTime alkuaika = ZonedDateTime.of(2021, 6, 9, 18, 0, ZoneId.of("Europe/Helsinki"));
ZonedDateTime loppuaika = ZonedDateTime.of(2021, 6, 11, 12, 0, ZoneId.of("Europe/Helsinki"));

// Tarkista, onko tapahtuma tapahtunut välillä
boolean onValiaika = tapahtumaAika.isAfter(alkuaika.toLocalDateTime()) && tapahtumaAika.isBefore(loppuaika.toLocalDateTime());
```

**Tulostus:**
```
onValiaika: true
```

## Katso myös

- [Java LocalDate Documentation](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/LocalDate.html)
- [Java ZonedDateTime Documentation](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/ZonedDateTime.html)
- [Java Duration Documentation](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/Duration.html)
- [Java Period Documentation](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/Period.html)