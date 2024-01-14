---
title:    "TypeScript: Merkkijonon pituuden selvittäminen"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Miksi

On monia tilanteita, joissa sinun täytyy tietää merkkijonon pituus TypeScriptissä. Tämä voi olla tarpeen esimerkiksi, kun validoit käyttäjän syötettä tai käsittelet tietoa tietokannasta.

## Miten

Merkkijonon pituuden löytäminen TypeScriptissä on helppoa. Voit käyttää sisäänrakennettua `.length` -ominaisuutta, joka palauttaa merkkijonon pituuden kokonaislukuna.

```TypeScript
let teksti: string = "Tämä on esimerkki";
console.log(teksti.length);
```

Tämä koodi tulostaa `18`, joka on merkkijonon `Tämä on esimerkki` pituus. Voit myös käyttää `.length` ominaisuutta yhdessä `console.log()` -funktion kanssa tulostaaksesi pituuden suoraan konsoliin.

```TypeScript
let teksti: string = "Tervetuloa TypeScript ohjelmointiin";
console.log("Merkkijonon pituus on: " + teksti.length);
```

Tämä koodi tulostaa `Merkkijonon pituus on: 32` konsoliin.

## Syvemmälle

Vaikka `.length` ominaisuus on helppo tapa löytää merkkijonon pituus, se voi aiheuttaa ongelmia, jos tekstissä on Unicode-merkkejä. Tämä johtuu siitä, että UTF-8 -koodausjärjestelmässä jokainen Unicode-merkki voi ottaa enintään 4 tavua, mikä voi vaikuttaa merkkijonon pituuteen.

Voit käyttää `TextEncoder()` -funktiota välttääksesi tämän ongelman. Tämä muuntaa tekstin tavupuskuriksi, jonka voit sitten laskea `.length` ominaisuudella.

```TypeScript
let teksti: string = "Tämä teksti sisältää Unicode-merkkejä 😊";
let tavupuskuri = new TextEncoder().encode(teksti);
console.log(tavupuskuri.length);
```

Tämä koodi tulostaa `40`, mikä on oikea pituus, vaikka tekstissä on Unicode-merkkejä.

## Katso myös

- [TypeScriptin merkkijonojen dokumentaatio](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- [TextEncoder() dokumentaatio](https://developer.mozilla.org/en-US/docs/Web/API/TextEncoder)