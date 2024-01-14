---
title:    "Java: Päivämäärän laskeminen tulevaisuuteen tai menneisyyteen"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

Tulevaisuuden tai menneen päivämäärän laskeminen voi olla tarpeellista esimerkiksi erilaisten tapahtumien ja juhlapäivien suunnittelussa tai tärkeiden muistopäivien selvittämisessä.

## Miten

Joskus on tarpeellista laskea tulevaisuuden tai menneen päivämäärän välillä kuluneiden päivien tai kuukausien määrä. Tätä varten Java-ohjelmointikielessä on valmiiksi sisäänrakennettu luokka nimeltä "Calendar". Alla olevassa esimerkissä luodaan kaksi eri päivämäärää ja lasketaan niiden välillä kuluneiden päivien määrä.

```Java
import java.util.Calendar;

public class DateCalculator {

  public static void main(String[] args) {

    // Luodaan kaksi päivämäärää kalenteri-oliona
    Calendar tulevaisuus = Calendar.getInstance();
    tulevaisuus.set(2022, Calendar.JULY, 1); // 1.7.2022
    Calendar menneisyys = Calendar.getInstance();
    menneisyys.set(2021, Calendar.JULY, 1); // 1.7.2021

    // Lasketaan välissä kuluneiden päivien määrä
    long paivat = ((tulevaisuus.getTimeInMillis() - menneisyys.getTimeInMillis()) / (1000 * 60 * 60 * 24));

    // Tulostetaan tulos
    System.out.println("Välissä on kulunut " + paivat + " päivää.");
  }
}
```

Tämän ohjelman tulostukseksi saadaan: "Välissä on kulunut 365 päivää." Huomaa, että ajankohtien välissä kuluneiden päivien laskeminen vaatii hieman matemaattista laskentaa ja aikayksiköiden muuntelua.

## Syvemmälle

Java-ohjelmoinnissa päivämäärien laskemiseen on muitakin vaihtoehtoja, kuten "Date" ja "LocalDate" -luokat. Näitä voi hyödyntää myös päivämäärien vertailussa ja erilaisten aikavyöhykkeiden huomioimisessa.

Toinen hyödyllinen toiminto on päivämäärämuotoilu, joka mahdollistaa päivämäärän esittämisen halutussa muodossa. Esimerkiksi "SimpleDateFormat" -luokka tarjoaa erilaisia vaihtoehtoja päivämäärän ja ajan esittämiseen.

## Katso myös

- [Java Calendar-luokka](https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html)
- [Java Date-luokka](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [Java LocalDate-luokka](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Java SimpleDateFormat-luokka](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)