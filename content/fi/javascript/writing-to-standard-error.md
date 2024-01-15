---
title:                "Tavalliselle virheelle kirjoittaminen"
html_title:           "Javascript: Tavalliselle virheelle kirjoittaminen"
simple_title:         "Tavalliselle virheelle kirjoittaminen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi
Kirjoittaminen standardivirheeseen on tärkeä osa Javascriptin ohjelmointia, koska se mahdollistaa virheiden hallinnan ja korjaamisen tehokkaammin. Se auttaa myös ohjelmoijaa ymmärtämään, mitä ohjelmassa tapahtuu ja mikä aiheuttaa mahdollisia ongelmia.

## Miten tehdä se
Koodiesimerkki alla näyttää, miten voi kirjoittaa standardivirheeseen `console.error()` komennolla.
```Javascript
try {
    // Tarkistetaan, onko käyttäjä täysi-ikäinen
    if (age < 18) {
        throw new Error("Käyttäjän tulee olla vähintään 18-vuotias.");
    }
    // Jos kaikki on kunnossa, jatketaan koodin suorittamista
    console.log("Tervetuloa sisään!");
} catch(error) {
    // Jos ikä ei ole oikea, kirjoitetaan virheilmoitus standardivirheeseen
    console.error("Virhe: " + error.message);
}
```
Tulostus:
```
Virhe: Käyttäjän tulee olla vähintään 18-vuotias.
```

## Syvempää tietoa
Kirjoittaminen standardivirheeseen auttaa ohjelmoijaa löytämään ja korjaamaan virheitä. Se on myös tärkeä osa testausta ja debuggausta. Standardivirheen lisäksi on myös mahdollista kirjoittaa tietoa muuhun konsoliin esimerkiksi `console.log()` komennolla.

## Katso myös
- [MDN Web Docs: console.error()](https://developer.mozilla.org/en-US/docs/Web/API/Console/error)
- [W3Schools: JavaScript Error Messages](https://www.w3schools.com/js/js_errors.asp)
- [ESLint: The Popular Code Quality Tool for JavaScript](https://eslint.org/)