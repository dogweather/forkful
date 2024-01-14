---
title:                "Javascript: Merkkijonon suurennus"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Miksi päästäisiin laskemaan merkkijonoa JavaScript-ohjelmointia käsittelevälle blogipostille?

Merkkijonon muokkaaminen on yksi yleisimmistä tehtävistä ohjelmoinnin maailmassa, ja päästäminen merkkijonon kirjoittamille faneille on yksi tapa tehdä siitä mielenkiintoisempaa.

## Miten

Merkkijonon muokkaamiseksi JavaScriptissä on monia eri vaihtoehtoja, mutta yksi yleisimmistä on merkkijonon muuttaminen suurilla kirjaimilla. Tämä voidaan tehdä käyttämällä 'toUpperCase' -funktiota yhdessä merkkijonon kanssa. Katso seuraava esimerkki:

```Javascript
let merkkijono = "tervetuloa";
let uusi_merkkijono = merkkijono.toUpperCase ();
console.log (uusi_merkkijono); // Tervetuloa
```

## Syväsukellus

Merkkijonon muuttaminen suurilla kirjaimilla ei vain tee tekstin ulkonäöstä erilaisen, vaan sillä voi myös olla käytännön käyttötarkoituksia. Esimerkiksi käyttäjätunnuksen ja salasanan tarkistamisessa suuriset kirjaimet eivät tarkista tapaa, jolla käyttäjä varmisti tekstin, joten on tärkeää muuttaa merkkijono suurimman mahdollisen kirjaimen kanssa ennen kuin vertaillaan sitä tallennettuihin arvoihin.

## Katso myös

- [JavaScript-tietosanakirja - merkkijonot](https://developer.mozilla.org/fi/docs/Web/JavaScript/Data_structures#String_type)
- [W3Schools - Merkkijonofunktio](https://www.w3schools.com/jsref/jsref_touppercase.asp)
- [JavaScript-merkkijonot ja niiden muokkaaminen](https://www.geeksforgeeks.org/javascript-fired-events/#string-manipulation)