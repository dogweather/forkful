---
title:                "Javascript: Kirjoittaminen standardivirheelle"
programming_language: "Javascript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi

Kirjoittaminen standardivirheeseen on tärkeä taito, jonka jokaisen ohjelmoijan tulisi omaksua. Se auttaa tunnistamaan ja korjaamaan virheitä, jotka eivät muuten tulisi ilmi tavallisista tulostuksista.

## Kuinka

Koodatessa on käytännöllistä tietää, kuinka kirjoittaa virheitä standardivirheeseen, jotta ne voidaan tunnistaa ja korjata mahdollisimman nopeasti. Selvennyksen vuoksi esittelemme tässä muutamia esimerkkejä, joiden avulla voit oppia tätä taitoa.

``` Javascript
// Esimerkki 1
console.error("Tässä on virhe!");

/*
Tulostaa seuraavan virheen standardivirheeseen:
"Console error: Tässä on virhe!"
*/

// Esimerkki 2
let numero = "kolme";

if (isNaN(numero)) {
  console.error(`${numero} ei ole numero.`);
}

/*
Tulostaa seuraavan virheen standardivirheeseen:
"Console error: kolme ei ole numero."
*/

// Esimerkki 3
function tarkistaSalasana(salasana) {
  if (salasana.length < 8) {
    console.error("Salasanan tulee olla vähintään 8 merkkiä pitkä.");
  }
}

tarkistaSalasana("salasana");

/*
Tulostaa seuraavan virheen standardivirheeseen:
"Console error: Salasanan tulee olla vähintään 8 merkkiä pitkä."
*/
```

## Syventyvä tarkastelu

Kirjoittaminen standardivirheeseen on tärkeä taito, jota tulisi harjoitella säännöllisesti. Se auttaa löytämään ja korjaamaan virheitä koodista, jotta se toimisi mahdollisimman sujuvasti. Lisäksi voit kirjoittaa haluamasi viestin standardivirheeseen, jolloin se voi toimia muistutuksena tulevia korjauksia varten. On myös tärkeää muistaa, että hyvä ohjelmointitaito sisältää myös virheiden tunnistamisen ja niiden korjaamisen.

## Katso myös

- [MDN Web Docs: console.error()](https://developer.mozilla.org/fi/docs/Web/API/Console/error)
- [W3Schools: JavaScript Errors - Throw and Try to Catch](https://www.w3schools.com/js/js_errors.asp)
- [FreeCodeCamp: Introduction to errors in JavaScript](https://www.freecodecamp.org/news/introduction-to-errors-in-javascript/)