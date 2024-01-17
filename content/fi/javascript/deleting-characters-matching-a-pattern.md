---
title:                "Mallia vastaavien merkkien poistaminen"
html_title:           "Javascript: Mallia vastaavien merkkien poistaminen"
simple_title:         "Mallia vastaavien merkkien poistaminen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Merkkijonojen poistaminen, jotka vastaavat tiettyä kuvioa, on yleistä ohjelmoinnissa. Tämä prosessi mahdollistaa helpomman ja tehokkaamman datan käsittelyn ja muokkaamisen.

## Kuinka tehdä:
Esimerkki merkkijonon poistamisesta, jossa haluamme poistaa kaikki numeroilla alkavat merkkijonot:
```Javascript
let string = "123 Java on mahtavaa!";
string = string.replace(/\d+/g, "");
console.log(string); // Tulostaa: " Java on mahtavaa!";
```

## Syväsukellus:
Merkkijonojen poistamisen historiallinen tausta voidaan jäljittää takaisin 1960-luvulle, jolloin awkingin ja kellerautonneen kehittivät ensimmäisen tietokantajärjestelmän, joka pystyi suorittamaan erilaisia ​​hakuja ja poistamaan suuria tietomääriä kerralla.

Nykyään on olemassa useita tapoja poistaa merkkijonoja, jotka vastaavat tiettyä kuvioa, kuten käyttämällä split () -toimintoa tai säännöllisiä lausekkeita. Split () -funktiolla voit jakaa merkkijonon tiettyyn kuvioon perustuen ja poistaa näin haluamasi osat. Säännölliset lausekkeet puolestaan ​​tarjoavat monipuolisempia mahdollisuuksia ehdottomasti vastaavien merkkijonojen poistoon.

## Katso myös:
- [MDN Web Docs: säännölliset lausekkeet](https://developer.mozilla.org/fi/docs/Web/JavaScript/Guide/Regular_Expressions)
- [W3Schools: split () -toiminto](https://www.w3schools.com/jsref/jsref_split.asp)