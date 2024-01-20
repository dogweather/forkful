---
title:                "Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
html_title:           "Java: Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
simple_title:         "Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Tulevaisuuden tai menneisyyden päivämäärän laskeminen: Java-ohjelmointi

## Mitä & Miksi?
Tulevaisuuden tai menneisyyden päivämäärän laskeminen tarkoittaa tietyn ajanjakson lisäämistä tai vähentämistä päivämäärästä. Ohjelmoijat tekevät tätä erilaisten sovellusten, kuten kalenterien, muistutusten tai tehtävien aikataulutuksen, tarpeisiin.

## Kuinka tehdä:
```java
import java.time.LocalDate;
import java.time.Period;

public class PvmLaskur {
	public static void main(String[] args) {
		LocalDate tänään = LocalDate.now();
		Period viikko = Period.ofWeeks(1);

		LocalDate tulevaisuus = tänään.plus(viikko);
		System.out.println("Viikon päästä on: " + tulevaisuus);

		LocalDate menneisyys = tänään.minus(viikko);
		System.out.println("Viikko sitten oli: " + menneisyys);
	}
}
```
Tulostus: 
```
Viikon päästä on: 2022-09-02
Viikko sitten oli: 2022-08-19
```

## Syvempi sukellus
Idea päivämäärän laskemiseen tuli jo ennen digitaalista aikakautta, jolloin ihmiset suunnittelivat aikataulujaan kalentereissa. Java-ohjelmointikielessä tätä helpottamaan on luotu LocalDate-luokka ja Period-luokka.

Muut menetelmät kuin Java's Period ja LocalDate ovat esimerkiksi Joda-Time kirjasto ja vanhempi Calendar-luokka, mutta ne ovat monimutkaisempia ja vähemmän intuitiivisia.

LocalDate- ja Period-luokkien käyttö on suoraviivaista. Tämä johtuu siitä, että ne on suunniteltu seuraamaan ISO-8601-kalenterisysteemin standardeja, joka on maailmanlaajuisesti tunnustettu päivämäärän ja ajan esitysmuoto.

## Katso myös
1. [Oracle's Java Documentation: LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
2. [Oracle's Java Documentation: Period](https://docs.oracle.com/javase/8/docs/api/java/time/Period.html)
3. [ISO-8601 Standard](https://www.iso.org/iso-8601-date-and-time-format.html)