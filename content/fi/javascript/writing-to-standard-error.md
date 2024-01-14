---
title:                "Javascript: Tietokoneohjelmoinnin artikkeli: Standardivirheen kirjoittaminen"
simple_title:         "Tietokoneohjelmoinnin artikkeli: Standardivirheen kirjoittaminen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä, miksi ohjelmoijat päätyvät kirjoittamaan standard error -virheviestejä. Yksi tärkeimmistä on löytää ja korjata mahdollisia virheitä koodissa, mikä auttaa varmistamaan ohjelman toiminnan ja tehokkuuden.

## Miten

Voit kirjoittaa standard error -virheviestejä käyttämällä ```console.error()``` -funktiota. Tämä ottaa vastaan ​​yksi parametrin, joka on viesti, jonka haluat näyttää. Voit myös käyttää ```console.debug()``` -funktiota, joka on tarkoitettu ohjelmoijille näyttämään tarkempia virheviestejä.

Esimerkiksi, jos haluat näyttää virheviestin käyttäjälle, voit käyttää seuraavaa koodia: 

```Javascript 
console.error("Valitettavasti jotain meni pieleen. Yritä uudelleen myöhemmin.");
```

Tämä koodi näyttäisi seuraavan virheviestin: "Valitettavasti jotain meni pieleen. Yritä uudelleen myöhemmin." 

Voit myös lisätä muuttujien arvoja virheviestiin käyttämällä merkintöjä, kuten ```${variableName}```. Tämä auttaa sinua tarkemmin selvittämään, mikä aiheutti virheen.

## Syvällisempi tarkastelu

Kirjoittaminen standard error -virheviestejä on tärkeä osa ohjelmointia ja auttaa sinua kehittämään parempia ohjelmia. Virheviestit auttavat sinua tunnistamaan ja korjaamaan koodin ongelmakohtia, mikä tekee ohjelmistostasi tehokkaamman ja luotettavamman. On myös tärkeää ottaa huomioon kohdeyleisösi ja käyttää selkeitä ja informatiivisia viestejä, jotta käyttäjät ymmärtävät helposti, mitä on tapahtunut ja miten virhe voidaan korjata.

## Katso myös

- [Mozilla Developer Network - Selaimen konsoli](https://developer.mozilla.org/fi/docs/Web/API/Console)
- [W3Schools - Console.error()](https://www.w3schools.com/jsref/met_console_error.asp)
- [JavaScript.info - The console object](https://javascript.info/browser-console)
- [Stack Overflow - How to write errors to standard error in JavaScript?](https://stackoverflow.com/questions/51813820/how-to-write-errors-to-standard-error-in-javascript)