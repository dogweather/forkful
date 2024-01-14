---
title:    "Java: Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Miksi
Tiedät varmasti, kuinka paljon voit säästää aikaa, kun tiedät tarkalleen, milloin jotain tapahtuu. Automaattisesti laskemalla tulevan tai menneen päivän tärkeät siirtymät voit suunnitella ja rakentaa sovelluksesi välttämättömiä ominaisuuksia ja logiikkaa. Joten, jos rakastat ohjelmointia, tämä on oikea artikkeli sinulle. Tässä opimme, kuinka voit laskea päivämäärän tulevaisuudessa tai menneisyydessä Java-ohjelmoinnin avulla.

## Kuinka
```Java
// Tämä on yksinkertainen Java-koodi, joka laskee päivämäärän tulevaisuudessa käyttäen Java Calendar -luokkaa
import java.util.Calendar; 

// Luo uusi Calendar-instanssi
Calendar tulevaPaiva = Calendar.getInstance(); 

// Aseta tuleva päivämäärä
tulevaPaiva.set(Calendar.YEAR, 2021);
tulevaPaiva.set(Calendar.MONTH, Calendar.DECEMBER); // Huomaa, että Java-luokka käyttää kuukaudet indeksoituina, eli joulukuu on 11.
tulevaPaiva.set(Calendar.DAY_OF_MONTH, 31);

// Tulosta tuleva päivämäärä
System.out.println("Tuleva päivämäärä on: " + tulevaPaiva.getTime()); // Tulostaa: Tuleva päivämäärä on: Fri Dec 31 00:00:00 EET 2021
```

Ja tässä on koodi, joka laskee päivämäärän menneisyydessä:

```Java
// Luo uusi Calendar-instanssi
Calendar menneinenPaiva = Calendar.getInstance(); 

// Aseta menneinen päivämäärä
menneinenPaiva.set(Calendar.YEAR, 1995);
menneinenPaiva.set(Calendar.MONTH, Calendar.AUGUST); // Elokuu on 7.
menneinenPaiva.set(Calendar.DAY_OF_MONTH, 17);

// Tulosta menneinen päivämäärä
System.out.println("Menneinen päivämäärä oli: " + menneinenPaiva.getTime()); // Tulostaa: Menneinen päivämäärä oli: Thu Aug 17 00:00:00 EEST 1995
```

Kuten näet, Java Calendar -luokka tarjoaa kätevät metodit päivämäärän manipulointiin. Voit myös laskea tulevan tai menneen ajanjakson ottamalla huomioon nykyisen päivämäärän käyttämällä `add` -metodia.

## Syväsukellus
Java-ohjelmoinnissa päivämäärän laskeminen on tärkeää useissa erilaisissa sovelluksissa, kuten kalenterisovelluksissa, laskutussovelluksissa ja varausjärjestelmissä. Java Calendar -luokka tarjoaa monipuoliset toiminnot päivämäärän manipulointiin, kuten tulevan tai menneen päivämäärän laskemisen, päivämäärän asettamisen ja päivämäärän lisäämisen tai vähentämisen tietyn ajanjakson verran.

On myös tärkeää huomata, että Java 8 sisältää uuden `LocalDate` -luokan, joka tarjoaa modernimman tavan käsitellä päiviä, kuukausia ja vuosia. `LocalDate` -luokka toimii helpommin ja suorituskykyisemmin kuin `Calendar` -luokka, joten sitä suositellaan käytettäväksi uusissa sovelluksissa.

## Katso myös
- [Java Calendar -luokan dokumentaatio](https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html)
- [Java 8 LocalDate -luokan dokumentaatio](https://docs.oracle.com/javase/8