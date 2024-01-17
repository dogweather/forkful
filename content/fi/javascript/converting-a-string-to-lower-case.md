---
title:                "Merkkijonon muuttaminen pienaakkosiksi"
html_title:           "Javascript: Merkkijonon muuttaminen pienaakkosiksi"
simple_title:         "Merkkijonon muuttaminen pienaakkosiksi"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Konvertointi merkkijonon pienikirjainmuotoon tarkoittaa merkkijonon muuttamista niin, että kaikki isot kirjaimet muutetaan vastaaviksi pieniksi kirjaimiksi. Ohjelmoijat tekevät tämän yleensä sen vuoksi, että he haluavat muuttaa merkkijonon tiedot yhtenäisempään muotoon, jolloin esimerkiksi hakutoiminnot ovat helpompia suorittaa.

## Kuinka?
```Javascript
// Esimerkki koodi
const merkkijono = "TÄMÄ ON ESIMERKKI";
const pienikirjainmuoto = merkkijono.toLowerCase();

console.log(pienikirjainmuoto);
// output: "tämä on esimerkki"
```

## Syvempää tietoa
1. Konvertointi merkkijonon pienikirjainmuotoon tuli käytettäväksi ensimmäisen kerran Unix-käyttöjärjestelmässä.
2. Vaihtoehtoisesti merkkijonon pienikirjainmuotoon voi muuttaa myös käyttämällä "String.toLowerCase" -funktiota.
3. Tämän toiminnon toteuttaminen tapahtuu lähdekoodissa usein käyttämällä "for" -silmukkaa ja muuttamalla jokainen kirjain erikseen.

## Katso myös
https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase