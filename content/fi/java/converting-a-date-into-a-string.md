---
title:    "Java: Päivämäärän muuttaminen merkkijonoksi"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi?

Monissa ohjelmointiprojekteissa on tarpeen muuttaa päivämäärä merkkijonoksi, jotta se voidaan helposti esittää käyttäjälle tai tallentaa tietokantaan. Tämä voi olla hyödyllistä esimerkiksi kalenteri-sovelluksissa tai laskutusjärjestelmissä. Tämä blogiteksti keskittyy siihen, kuinka muutat päivämäärän merkkijonoksi käyttäen Java-kielen ominaisuuksia.

## Kuinka tehdä se?

Java tarjoaa useita erilaisia tapoja muuttaa päivämäärä merkkijonoksi, mutta yksi yksinkertaisimmista tavoista on käyttää SimpleDateFormat-luokkaa. Tämä luokka mahdollistaa päivämäärän muuttamisen halutunmuotoiseksi merkkijonoksi. Alla on esimerkki koodista, joka muuttaa tänään olevan päivämäärän merkkijonoksi muodossa "dd/MM/yyyy".

```Java
SimpleDateFormat formatter = new SimpleDateFormat("dd/MM/yyyy");
Date today = new Date();
String dateStr = formatter.format(today);
System.out.println(dateStr);

```
Tulostus: 18/06/2021

Tässä esimerkissä käytetään yksinkertaista päivämäärän muotoiluohjetta "dd/MM/yyyy", mutta SimpleDateFormat-luokka tarjoaa myös muita vaihtoehtoja, kuten kirjainten lisäämisen merkiksi kuukauden tai viikonpäivän nimen näyttämistä varten. Voit lukea lisää SimpleDateFormat-luokasta Javan virallisesta dokumentaatiosta.

## Syvällinen sukellus

Päivämäärän muuttaminen merkkijonoksi voi olla myös herkkä aihe, sillä päivämäärät ja aikavyöhykkeet voivat aiheuttaa haasteita ohjelmoijille. Java tarjoaa kuitenkin kattavia ja turvallisia tapoja käsitellä päivämääriä. Tässä on joitain tärkeitä asioita, jotka kannattaa muistaa:

- Java 8:ssa esiteltiin uusi LocalDate-luokka, joka tarjoaa helpomman tavan käsitellä päivämääriä ilman aikavyöhykkeiden ongelmia. Tämä on erityisen hyödyllistä, jos työskentelet Java 8:lla ja uudemmilta versioilta. Voit lukea lisää LocalDate-luokasta täältä.

- Jos haluat luoda päivämäärän tietystä aikavyöhykkeestä, voit käyttää Calendar-luokan sijaan ZonedDateTime-luokkaa. Tämä luokka ottaa huomioon aikavyöhykkeet ja tarjoaa tarkemman muunnostavan.

- On tärkeää käsitellä päivämääriä turvallisesti, sillä käyttäjät voivat antaa syötteessään erilaisia päivämäärämalleja, jotka voivat aiheuttaa virheitä. Voit käyttää try-catch-lausekkeita tai Java 8:n Optional-luokkaa, joka tarjoaa turvallisemman tavan käsitellä mahdollisesti tyhjiä päivämääräarvoja.

## Katso myös

- [Java SimpleDateFormat -dokumentaatio](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [Java LocalDate -dokumentaatio](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Java ZonedDateTime -dokumentaatio](https://docs.oracle.com/javase/8/docs/api/java/time/ZonedDateTime.html)
- [Java Optional -dokumentaatio](https://docs.oracle.com/javase/8/docs/api/java/util/Optional.html)