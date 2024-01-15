---
title:                "Merkkijonon muuntaminen pieniksi kirjaimiksi"
html_title:           "Javascript: Merkkijonon muuntaminen pieniksi kirjaimiksi"
simple_title:         "Merkkijonon muuntaminen pieniksi kirjaimiksi"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi

Miksi joku haluaisi muuttaa merkkijonon pienaakkosiksi? On monia tilanteita, joissa tämä voi olla hyödyllistä, kuten tarkistettaessa käyttäjän syöttöä tai vertailtaessa merkkijonoja.

## Miten tehdä se

```Javascript
let name = "JOHN DOE";
console.log(name.toLowerCase());
```

Tuloste: john doe

Merkkijonon muuttaminen pienaakkosiksi tehdään käyttäen `.toLowerCase()` -metodia. Tämä muuttaa kaikki merkkijonon kirjaimet pieniksi.

## Syvemmälle

JavaScriptin `.toLowerCase()` -metodi perustuu Unicode-merkistöön, joten se toimii eri kielillä ja erikoismerkeillä. Tämän avulla voidaan varmistaa, että merkkijonot toimivat oikein myös kansainvälisissä sovelluksissa.

## Katso myös

- [JavaScript String Methods](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String)
- [Unicode Character Database](https://www.unicode.org/ucd/)