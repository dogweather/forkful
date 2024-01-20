---
title:                "Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
html_title:           "Javascript: Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
simple_title:         "Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Mikä ja Miksi?

Tulevaisuuden tai menneisyyden päivämäärän laskeminen tarkoittaa päivämäärän määräämistä tulevaisuuteen tai menneisyyteen tietystä aloituspisteestä. Ohjelmoijat tekevät tämän usein esimerkiksi aikarajoitusten hallintaan tai erityisten tapahtumien ajastamiseen.

## Kuinka Tehdä:

```Javascript
// Luodaan uusi Päivämäärä-objekti - oletuksena se on tämä päivämäärä.
let nykyPvm = new Date();

// Lisäämme viisi päivää nykyiseen päivämäärään.
nykyPvm.setDate(nykyPvm.getDate() + 5);

// Tulostaa päivämäärän 5 päivän päässä tulevaisuudessa
console.log(nykyPvm);
```

Samalla tavalla voit laskea päivämäärän menneisyydessä:

```Javascript
let nykyPvm = new Date();

// Vähennämme viisi päivää nykyisestä päivämäärästä.
nykyPvm.setDate(nykyPvm.getDate() - 5);

// Tulostaa päivämäärän 5 päivää sitten
console.log(nykyPvm);
```
## Syvällisempi Sukellus:

(1) **Historiallinen tausta**: JavaScriptin Date-objektin käyttäminen päivämäärien laskemiseksi on ollut mahdollista lähes siitä lähtien, kun JavaScript otettiin käyttöön vuonna 1995. Se on integroitu osa JavaScript-ympäristöä.

(2) **Vaihtoehdot**: Käytettävissä olevia kirjastoja, kuten Moment.js tai Day.js, voidaan käyttää saman toiminnallisuuden saavuttamiseksi, mutta ne tarjoavat useita muitakin päivämäärään ja aikaan liittyviä toimintoja.

(3) **Toteutuksen tiedot**: JavaScriptin Date-objektin setDate- ja getDate-metodien avulla voidaan laskea päiviä liikkumalla eteen- tai taaksepäin. Nämä metodit huomioivat kuukauden vaihtumisen ja karkausvuodet automaattisesti.

## Katso Myös:

- [JavaScriptin Date-objektin dokumentaatio MDN-web-sivustolla](https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js-kirjaston kotisivu](https://momentjs.com/)
- [Day.js-kirjaston kotisivu](https://day.js.org/)