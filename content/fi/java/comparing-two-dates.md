---
title:                "Java: Kahden päivämäärän vertailu"
programming_language: "Java"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Miksi Vertailla Aikoja?

Java on yksi suosituimmista ohjelmointikielistä maailmassa, ja yksi sen monista käyttökohteista on aikojen hallinta ja vertailu. Aikojen vertailulla on monia käyttötarkoituksia, kuten esimerkiksi tarkistaa, kumpi aika on tulevaisuudessa tai mikä on kahden tapahtuman välillä kuluneen ajan määrä. Tässä blogikirjoituksessa keskitymme tarkastelemaan, miten voit vertailla kahta päivämäärää Java-ohjelmoinnissa.

## Miten Vertailla Aikoja?

Java sisältää valmiita luokkia, jotka helpottavat aikojen vertailua. Yksi näistä luokista on `LocalDate`, joka sisältää päivämäärän ilman aikatietoja. Voit luoda uuden päivämäärän `LocalDate.of()` -metodilla ja antamalla sille päivämäärän komponentit vuosi, kuukausi ja päivä.

```Java
LocalDate date1 = LocalDate.of(2021, 10, 15);
LocalDate date2 = LocalDate.of(2021, 10, 20);
```

Voit sitten verrata näitä kahta päivämäärää käyttämällä `isBefore()` ja `isAfter()` -metodeja. Esimerkiksi:

```Java
if (date1.isBefore(date2)) {
  System.out.println(date1 + " on ennen " + date2);
}

if (date2.isAfter(date1)) {
  System.out.println(date2 + " on jälkeen " + date1);
}
```

Tämän koodin tulostus olisi:

```
2021-10-15 on ennen 2021-10-20
2021-10-20 on jälkeen 2021-10-15
```

Voit myös käyttää `isEqual()` -metodia tarkistaaksesi, ovatko päivämäärät samat. Lisäksi voit käyttää `compareTo()` - metodia, joka palauttaa negatiivisen, nollan tai positiivisen luvun riippuen siitä, onko ensimmäinen päivämäärä ennen, sama kuin vai jälkeen toisen päivämäärän.

## Syvällisesti Aikojen Vertailusta

Aikojen vertailu sisältää myös aikatietojen, kuten tuntien, minuuttien ja sekuntien huomioon ottamisen. Tätä varten Java tarjoaa `LocalDateTime` -luokan, joka sisältää myös aikatiedot. Voit luoda uuden ajan käyttämällä `LocalDateTime.of()` -metodia, joka hyväksyy myös aikatiedot parametreina.

```Java
LocalDateTime dateTime1 = LocalDateTime.of(2021, 10, 15, 12, 30, 45);
LocalDateTime dateTime2 = LocalDateTime.of(2021, 10, 15, 13, 30, 45);
```

Voit sitten käyttää samoja metodeja ja vertailuja kuin `LocalDate` -luokan kanssa, mukaan lukien `isEqual()`, `isBefore()` ja `isAfter()`.

## Katso Myös

* [Java-tutoriaali: LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
* [Java-tutoriaali: LocalDateTime](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDateTime.html)