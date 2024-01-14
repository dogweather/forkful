---
title:                "Java: Etäisen tai menneen päivämäärän laskeminen tietokoneohjelmoinnissa"
programming_language: "Java"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

Jos olet koskaan tarvinnut laskea tietyn päivämäärän tulevaisuudessa tai menneisyydessä, tämä blogikirjoitus on sinulle! Java-ohjelmointikielen avulla voit helposti laskea tulevia tai menneitä päivämääriä haluamallasi tavalla.

## Kuinka tehdä

Aloitetaan yksinkertaisimmasta tapauksesta: haluat laskea päivämäärän 30 päivää eteenpäin nykyisestä päivästä. Tätä varten voit käyttää Calendar-luokkaa ja sen add-metodia. Alla on esimerkki:

```Java
Calendar calendar = Calendar.getInstance();
calendar.add(Calendar.DATE, 30);
System.out.println("Päivämäärä 30 päivää tulevaisuudessa on " + calendar.getTime());
```

Tulostus näyttäisi tältä:

```
Päivämäärä 30 päivää tulevaisuudessa on Thu Nov 10 15:30:00 EET 2021
```

Voit myös laskea tulevia tai menneitä päivämääriä eri yksiköissä, kuten kuukausina tai vuosina. Alta löydät esimerkin, jossa lasketaan päivämäärä 2 kuukautta aiemmin nykyisestä päivästä:

```Java
Calendar calendar = Calendar.getInstance();
calendar.add(Calendar.MONTH, -2);
System.out.println("Päivämäärä 2 kuukautta menneisyydessä on " + calendar.getTime());
```

Tulostus näyttäisi tältä:

```
Päivämäärä 2 kuukautta menneisyydessä on Thu Sep 10 15:30:00 EET 2021
```

Voit myös käyttää LocalDate-luokkaa, joka on osa Java 8:n uutta Java Time APIa. Alla olevassa esimerkissä käytämme sen plusDays-metodia laskeaksemme päivämäärän 10 päivää tulevaisuudessa:

```Java
LocalDate today = LocalDate.now();
LocalDate futureDate = today.plusDays(10);
System.out.println("Päivämäärä 10 päivää tulevaisuudessa on " + futureDate);
```

Tulostus näyttäisi tältä:

```
Päivämäärä 10 päivää tulevaisuudessa on Fri Oct 15 15:30:00 EET 2021
```

Nämä ovat vain muutamia esimerkkejä siitä, kuinka voit laskea tulevia ja menneitä päivämääriä Java-ohjelmointikielellä. Voit kokeilla erilaisia yksiköitä ja erilaisia tapoja laskea päiväsi. Muista myös tarkistaa Java Time API:n muut hyödylliset luokat, kuten Period ja Duration.

## Syvällisempi kuvaus

Edellä mainitut esimerkit antavat sinulle perustiedot siitä, kuinka voit laskea tulevia ja menneitä päivämääriä Java-ohjelmointikielellä. On kuitenkin tärkeää huomata, että päivämäärä lasketaan aina nykyisestä ajasta. Jos haluat laskea päivämäärää tietystä päivämäärästä, sinun on ensin muunnettava se LocalDate- tai Calendar-olmuksi.

Lisäksi Java Time API sisältää monia muita hyödyllisiä luokkia päivämäärien ja aikojen manipulointiin. Kannattaa tutustua niihin syvällisempää ymmärrystä varten.

## Katso myös

- [Oracle Java Time API:n virallinen dokumentaatio](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Java Date and Time -tutoriaali](https://www.baeldung.com/java-date-time)
- [Java Calendar -luok