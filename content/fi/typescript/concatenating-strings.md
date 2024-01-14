---
title:    "TypeScript: Lausekkeiden yhdistäminen"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Miksi?

Miksi joku haluaisi yhdistää merkkijonoja TypeScript-ohjelmoinnissa? Yhdistäminen on välttämätöntä, kun haluat luoda uuden merkkijonon yhdistämällä useita erillisiä merkkijonoja.

## Miten tehdä?

```TypeScript
//Esimerkki 1
let opiskelijat = "Matti";
let koulu = "Aalto-yliopisto";
let lopullinenMerkkijono = opiskelijat + " opiskelee " + koulu;
console.log(lopullinenMerkkijono);
// tulostaa: "Matti opiskelee Aalto-yliopisto"

//Esimerkki 2
let luku = 123;
let merkkijono = "Tämä on luku: " + luku.toString();
console.log(merkkijono);
// tulostaa: "Tämä on luku: 123"
```

Yllä olevissa esimerkeissä näemme, että merkkijonojen yhdistäminen tapahtuu käyttämällä `+` -operaattoria. Voimme myös käyttää `.toString()` -metodia, jotta voimme yhdistää merkkijonon ja muun datan, kuten numeron.

## Syvällisempi tarkastelu

Merkkijonon yhdistäminen voi vaikuttaa yksinkertaiselta, mutta se voi aiheuttaa ongelmia, jos sitä ei käytetä oikein. Esimerkiksi, jos yritämme yhdistää merkkijono ja muuttujan, joka on `undefined`, tuloksena on "undefined" -sana, mikä ei ehkä ole haluttu tulos. On tärkeää varmistaa, että kaikki yhdistettävät arvot ovat halutussa muodossa, jotta lopullinen merkkijono olisi oikein.

Toinen asia, jota on syytä huomata, on merkkijonojen ja numeroiden yhdistäminen. Vaikka tarvittaessa pystymme käyttämään `.toString()` -metodia, on parempi käyttää `Template literals` -ominaisuutta, joka on TurboScriptin syntaksi, joka auttaa yhdistämään erilaisia arvoja ilman monimutkaista koodia.

```TypeScript
//Esimerkki 3
let luku = 123;
let merkkijono = `Tämä on luku: ${luku}`;
console.log(merkkijono);
// tulostaa: "Tämä on luku: 123"
```

## Katso myös

- [TypeScriptin virallinen dokumentaatio](https://www.typescriptlang.org/docs/home.html)
- [Tutorialspointin TypeScript-esittely](https://www.tutorialspoint.com/typescript/)
- [TypeScript vs JavaScript: Mikä niiden ero on?](https://stackoverflow.com/questions/12694530/typescript-for-beginners#12695152)