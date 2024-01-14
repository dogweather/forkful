---
title:                "Javascript: Säännöllisten lausekkeiden käyttö"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi käyttää regular expressioneja?

Regular expressionit ovat erittäin hyödyllinen työkalu ohjelmoinnin maailmassa. Ne auttavat suorittamaan monimutkaisia hakukuvioita ja korvaamaan sekä muokkaamaan tekstiä tehokkaasti. Ne myös säästävät aikaa, kun etsitään tiettyjä merkkijonoja koodin joukosta.

## Miten käyttää regular expressioneja?

```Javascript
// Etsitään sanaa "tietokone" tekstistä ja tulostetaan sen indeksi
const teksti = "Haluaisin ostaa uuden tietokoneen.";
const regex = /tietokone/g;
console.log(teksti.search(regex)); // output: 21
```

Tässä esimerkissä käytämme `search`-funktiota yhdessä regular expressionin kanssa löytääksemme ensimmäisen esiintymän sanasta "tietokone". Huomaa myös sana "g" regular expressionin lopussa, mikä tarkoittaa "globaalia" eli sitä, että haluamme löytää kaikki esiintymät.

```Javascript
// Korvataan sanat "tietokone" sanalla "läppäri"
const uusiTeksti = teksti.replace(regex, "läppäri");
console.log(uusiTeksti); // output: Haluaisin ostaa uuden läppärin.
```

Tässä käytämme `replace`-funktiota korvataksemme kaikki esiintymät sanasta "tietokone" sanalla "läppäri". Näin voimme helposti muokata tekstiä tarpeidemme mukaan.

## Syväsukellus regular expressioneihin

Regular expressioneja käyttäessä on tärkeää muistaa, että ne ovat erittäin tarkkoja ja pienikin virhe voi johtaa epätoivottuihin tuloksiin. Tästä syystä on hyvä lukea lisää niiden syntaksista ja toiminnasta ennen käyttöönottoa. On myös hyödyllistä kokeilla ja testata regular expressioneja eri tilanteissa, jotta niiden käyttö tulee luontevaksi.

## Katso myös

- [W3Schools: Regular Expressions](https://www.w3schools.com/jsref/jsref_obj_regexp.asp)
- [MDN web docs: Regular Expressions Guide](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RegExp)
- [Regexr: Learn, Build & Test RegEx](https://regexr.com/)