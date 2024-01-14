---
title:    "Javascript: Säännöllisten lausekkeiden käyttö"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi käyttää säännöllisiä lausekkeita?

Säännölliset lausekkeet ovat tehokas työkalu JavaScript-ohjelmoijille, jotka haluavat hakea ja manipuloida merkkijonoja. Ne voivat auttaa sinua lyhentämään koodin määrää ja tehostamaan suorituskykyä. 

## Miten aloittaa

Säännöllisillä lausekkeilla on erityinen syntaksi, joka mahdollistaa merkkijonojen hakemisen ja muokkaamisen. Yksinkertainen esimerkki on hakea kaikki sanat, jotka alkavat kirjaimella "a".

```Javascript
let merkkijono = "Aloitetaan koodaus säännöllisillä lausekkeilla.";

let haku = merkkijono.match(/\ba\w*/g);

console.log(haku);
// Output: ["Aloitetaan", "a", "säännöllisillä"]
```

Ensimmäisellä rivillä luomme muuttujan ja määritämme siihen merkkijonon, josta haluamme hakea sanoja. Toisella rivillä käytämme `match`-metodia säännölliselle lausekkeelle ja tallennamme tuloksen muuttujaan `haku`. Lopuksi tulostamme tuloksen konsoliin.

## Syvällinen sukellus

Säännölliset lausekkeet koostuvat erikoismerkeistä ja merkkipaljastimista, joiden avulla voit luoda monimutkaisempia hakuja. Alla on muutamia yleisiä esimerkkejä.

- `.`: Mikä tahansa merkki paitsi uusi rivi
- `^`: Merkkijonon alku
- `$`: Merkkijonon loppu
- `[]`: Määritä merkkijono, jonka haluat hakea
- `[-]`: Määritä merkkijonoalue, jonka sisältä haluat hakea
- `\w`: Pienet ja isot kirjaimet sekä numerot
- `\W`: Kaikki muu kuin `\w`

Katso lisää esimerkkejä ja tarkempaa syventymistä [MDN:n sivulta](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions).

## Katso myös

- [RegExp Objektin dokumentaatio](https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Global_Objects/RegExp)
- [Regex101 - Työkalu säännöllisten lausekkeiden testaamiseen](https://regex101.com/)
- [JavaScript Regex Cheat Sheet](https://www.debuggex.com/cheatsheet/regex/javascript)