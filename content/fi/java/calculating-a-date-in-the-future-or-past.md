---
title:                "Java: Tulevaisuuden tai menneen päivämäärän laskeminen"
simple_title:         "Tulevaisuuden tai menneen päivämäärän laskeminen"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi
Päivämäärän laskeminen tulevaisuuteen tai menneisyyteen voi olla hyödyllistä esimerkiksi sovelluksissa, joissa tarvitaan muistutuksia tai aikarajoja. Se voi myös auttaa ymmärtämään päivämäärien välisiä eroja ja aikojen laskemista.

## Kuinka
Seuraavassa näytetään esimerkki siitä, kuinka voit ohjelmallisesti laskea tietyn päivämäärän tietyn ajanjakson päässä tai menneisyydessä. Koodiblokissa näet esimerkin Java-koodista ja sen tulosteen.

```Java
// Tulevaisuuden päivämäärän laskeminen
LocalDate tulevaisuus = LocalDate.now().plusDays(30);
System.out.println("Päivämäärä 30 päivää tulevaisuudessa: " + tulevaisuus);

// Menneisyyden päivämäärän laskeminen
LocalDate menneisyys = LocalDate.now().minusMonths(6);
System.out.println("Päivämäärä 6 kuukautta menneisyydessä: " + menneisyys);
```

Tämä koodi laskee nykyisen päivämäärän perusteella tulevan päivämäärän 30 päivän päässä ja menneisyydessä olevan päivämäärän 6 kuukauden päässä. Tulosteen pitäisi olla jotain seuraavaa:

```
Päivämäärä 30 päivää tulevaisuudessa: 2019-05-08
Päivämäärä 6 kuukautta menneisyydessä: 2018-11-08
```

Voit myös vaihtaa laskettavan ajanjakson päivien, kuukausien tai vuosien sijasta käyttämällä esimerkiksi `plusMonths()` tai `minusYears()` -metodia.

## Syvällinen sukellus
Java tarjoaa monia hyödyllisiä luokkia ja metodeja päivämäärän laskemiseen. Esimerkiksi `LocalDate`-luokka sisältää metodeja päivämäärien lisäämiseen ja vähentämiseen. Lisäksi `LocalDateTime`-luokka sisältää myös ajan huomioivia metodeja, kuten `plusHours()` ja `minusMinutes()`.

Päivien, kuukausien ja vuosien lisäämisen tai vähentämisen lisäksi voit myös asettaa tietyn päivämäärän käyttämällä `with()`-metodia. Tämä voisi olla hyödyllistä esimerkiksi syntymäpäivän asettamisessa tietokannassa.

## Katso myös
- [Java 8 DateTime API](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Stack Overflow: How to manipulate LocalDate](https://stackoverflow.com/questions/39896487/how-to-manipulate-localdate-in-java)