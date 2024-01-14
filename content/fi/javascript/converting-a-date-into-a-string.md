---
title:                "Javascript: Päivämäärän muuntaminen merkkijonoksi"
simple_title:         "Päivämäärän muuntaminen merkkijonoksi"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Joskus JavaScript-ohjelmoinnissa halutaan muuttaa päivämäärä merkkijonoksi. Tämä voi johtua esimerkiksi tarpeesta tallentaa päivämäärä tietokantaan tai näyttää se käyttäjälle.

## Kuinka

Päivämäärän muuttaminen merkkijonoksi JavaScriptissä on yksinkertaisimmillaan helposti ymmärrettävän `toString()` -funktion avulla. Tämä muuntaa päivämäärän haluttuun merkkijonoesitykseen.

```JavaScript
const date = new Date();
const dateString = date.toString();
console.log(dateString);
// Tulostaa esimerkiksi "Fri May 28 2021 17:30:23 GMT+0300 (Eastern European Summer Time)"
```

Voit myös muokata merkkijonoesitystä oman halusi mukaan käyttämällä Date-objektin tarjoamia metodeja. Esimerkiksi `toLocaleDateString()` tai `toLocaleTimeString()` voivat olla hyödyllisiä haluttaessa esittää päivämäärä tai aika tiettyyn paikalliseen aikaan.

```JavaScript
const date = new Date();
const dateString = date.toLocaleDateString("fi-FI"); // Suomalainen päivämäärämuoto
const timeString = date.toLocaleTimeString("fi-FI"); // Suomalainen aikamuoto
console.log(`${dateString} klo ${timeString}`);
// Tulostaa esimerkiksi "28.5.2021 klo 17:30:23"
```

## Syvempi sukellus

Tarkemmin määriteltynä `toString()`-funktio palauttaa päivämäärän merkkijonoesityksen RFC 1123 -standardin mukaisesti. Tämä tarkoittaa sitä, että päivämäärä muutetaan Greenwichin keskimääräiseen aikaan (GMT) ja näytetään tiettyyn aikavyöhykkeeseen lisätyn tai poistetun aikamäärän kera.

Jos haluat hallita päivämäärän esitystapaa tarkemmin, voit käyttää `Date`-objektin tarjoamia muita metodeja, kuten `toUTCString()` tai `toISOString()`. Näiden avulla voit muuntaa päivämäärän haluamaasi muotoon tai jopa luoda oman mukautetun merkkijonoesityksen.

## Katso myös

- [MDN Web Docs - Date-objekti](https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Modernin JavaScriptin opas: Date](https://javascript.info/date)
- [W3Schools - JavaScript Date-objekti](https://www.w3schools.com/js/js_dates.asp)