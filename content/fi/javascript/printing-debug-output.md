---
title:                "Virheilmoituksen tulostaminen"
html_title:           "Javascript: Virheilmoituksen tulostaminen"
simple_title:         "Virheilmoituksen tulostaminen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Tulostus debug-tulosteeseen on ohjelmoinnin tapa lähettää tietoja ohjelman suorituksen aikana, jotta kehittäjät voivat havaita mahdollisia virheitä tai ongelmia. Tämä auttaa heitä korjaamaan koodia ja parantamaan ohjelmiston suorituskykyä.

## Kuinka tehdä:
Käytä Javascriptillä console.log()-funktiota tulostamaan viestejä debug-tulosteeseen. Voit myös käyttää console.error()-funktiota virheiden tulostamiseen tai console.warn()-funktiota varoituksien tulostamiseen. Esimerkiksi:

```
let nimi = "Matti";
console.log("Hei " + nimi);
```

Tämä tulostaisi debug-tulosteeseen "Hei Matti".

## Syvemmälle:
Tulostus debug-tulosteeseen on tärkeä ominaisuus kaikille ohjelmointikielille, ja Javascript ei ole poikkeus. Se auttaa kehittäjiä tunnistamaan ja korjaamaan virheitä sekä parantamaan koodin suorituskykyä. On myös muita tapoja tulostaa debug-tulosteeseen, kuten käyttämällä konsolia selaimessa tai erityisiä debuggaus- ja kehitystyökaluja.

## Katso myös:
- [MDN Web Docs - Debugging JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Debugging)
- [W3Schools - JavaScript Debugging](https://www.w3schools.com/js/js_debugging.asp)