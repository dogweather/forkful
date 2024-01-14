---
title:                "Javascript: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluat käyttää Javascriptin hakua ja tekstin korvaamista? Monissa koodaustilanteissa saattaa olla tarpeellista korvata esimerkiksi tietynlaiset merkit tai tekstit toisilla. Tässä blogikirjoituksessa käymme läpi, miten tämä onnistuu Javascript-ohjelmoinnissa.

## Miten tehdä

Javascriptissa on kätevä funktio nimeltään replace(), jota voi käyttää tekstin hakemiseen ja korvaamiseen. Se ottaa vastaan kaksi parametria: etsittävän tekstin ja korvaavan tekstin. Lopuksi funktio palauttaa muokatun tekstin.

```Javascript
let teksti = "Tervetuloa suomeen!";
let uusiTeksti = teksti.replace("suomeen", "Suomeen");

console.log(uusiTeksti);

// Output: Tervetuloa Suomeen!
```

Hakeminen ja korvaaminen voi tapahtua myös säännöllisten lausekkeiden avulla, mikä antaa enemmän mahdollisuuksia muokata tekstejä halutulla tavalla.

```Javascript
let lauseke = /elon/i // hakee kaikki esiintymät, jotka sisältävät "elon" sanaan katsomatta piirroksia ( case-insensitive)
let teksti = "Elon Musk on menestynyt liikemies ja keksijä.";
let uusiTeksti = teksti.replace(lauseke, "Jeff Bezos");

console.log(uusiTeksti);

// Output: Jeff Bezos on menestynyt liikemies ja keksijä.
```

## Syvällinen sukellus

Javascriptin replace() -funktiota käyttämällä voi myös korvata tekstin tietyn paikan perusteella tietyn kerran tai jopa kaikki esiintymät. Esimerkiksi, jos haluat korvata vain ensimmäisen esiintymän, voit antaa parametrina vaihtoehdon "1".

```Javascript
let teksti = "Olen surffannut kolme kertaa tänä kesänä.";
let uusiTeksti = teksti.replace("kolme", "viisi", 1);

console.log(uusiTeksti);

// Output: Olen surffannut viisi kertaa tänä kesänä.
```

Voit myös korvata kaikki esiintymät käyttämällä globaalia vaihtoehdoissa, mikäli tarvetta esiintyy.

```Javascript
let teksti = "Javascript on hieno ohjelmointikieli, joka yhä kasvattaa suosiotaan.";
let uusiTeksti = teksti.replace(/ohjelmointikieli/ig, "koodauskieli");

console.log(uusiTeksti);

// Output: Javascript on hieno koodauskieli, joka yhä kasvattaa suosiotaan.
```

## Katso myös

- [MDN Web Docs - String.prototype.replace()](https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [W3Schools - JavaScript String replace() Method](https://www.w3schools.com/jsref/jsref_replace.asp)
- [Eloquent JavaScript - Regular Expressions](https://eloquentjavascript.net/09_regexp.html)