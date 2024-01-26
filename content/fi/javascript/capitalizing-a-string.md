---
title:                "Merkkijonon muuttaminen isoiksi kirjaimiksi"
html_title:           "Arduino: Merkkijonon muuttaminen isoiksi kirjaimiksi"
simple_title:         "Merkkijonon muuttaminen isoiksi kirjaimiksi"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä ja Miksi?)
Kirjainkokojen muuttaminen tarkoittaa sanan tai lauseen alkukirjaimen muuttamista isoksi. Tämä parantaa lukemisen selkeyttä ja korostaa tärkeät nimet tai otsikot.

## How to (Kuinka toteuttaa):
```Javascript
// Esimerkki 1: Yksinkertainen tapa muuttaa ensimmäinen kirjain isoksi
function capitalizeFirstLetter(string) {
  return string.charAt(0).toUpperCase() + string.slice(1);
}

console.log(capitalizeFirstLetter('mökki')); // Output: Mökki

// Esimerkki 2: Kaikkien sanojen ensimmäisen kirjaimen muuttaminen isoksi
function capitalizeWords(string) {
  return string.split(' ').map(word => capitalizeFirstLetter(word)).join(' ');
}

console.log(capitalizeWords('hyvää huomenta!')); // Output: Hyvää Huomenta!
```

## Deep Dive (Syväkatsaus):
Ennen kuin oli valmiita funktioita, kehittäjät joutuivat käsin käsittelemään merkkijonoja. Historiallisesti merkkijonokäsittely on aina ollut ohjelmoinnin keskeinen osa. JavaScriptissä ei ole sisäänrakennettua toimintoa joka suoraan muuntais koko merkkijonon alkukirjaimet isoiksi, mutta yllä olevat ratkaisut ovat tulleet standardiksi.

Vaihtoehtoisesti voisi käyttää CSS:n `text-transform: capitalize;` ominaisuutta, mutta se vaikuttaa vain näyttötapaan, ei itse merkkijonon dataan.

Toteutuksen kannalta tärkeää on muistaa, että JavaScriptissä merkkijono on muuttumaton (immutable), eli alkuperäistä merkkijonoa ei voi muuttaa - sen sijaan luodaan uusi merkkijono kun halutaan tehdä muutoksia.

## See Also (Katso Myös):
- MDN String.prototype.toUpperCase(): https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase
- MDN String.prototype.charAt(): https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/charAt
- MDN Array.prototype.map(): https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/map
