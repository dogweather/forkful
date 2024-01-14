---
title:    "Javascript: Merkkijonon pituuden löytäminen"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisimme selvittää merkkijonon pituuden? No, siinä on monia syitä! Saatat haluta varmistaa, että merkkijono sopii tiettyyn kenttään tai tarvitset tietoa sen pituudesta käsitelläksesi sitä eri tavalla. 

## Miten

JavaScriptissä merkkijonon pituus voidaan selvittää käyttämällä `.length`-ominaisuutta. Tämä ominaisuus antaa meille merkkijonon merkkien määrän. Otetaan esimerkiksi seuraava koodinpätkä:

```Javascript
let string = "Hei, maailma!";
console.log(string.length); // Output: 12
```

Kuten näet, `.length`-ominaisuuden avulla voimme helposti selvittää merkkijonon pituuden ja käyttää sitä tarvittaessa koodissamme. 

## Syvällisempi sukellus

On tärkeää huomata, että JavaScriptissä merkkijonon pituus lasketaan mukaan myös välilyönnit ja välimerkit. Tämä tarkoittaa sitä, että jos haluat selvittää pelkästään kirjainten määrän merkkijonossa, sinun tulee tehdä lisäkäsittelymerkintä. 

Voit myös käyttää merkkijonon `.slice()`-metodia saadaksesi tietyt merkkijonon osat ja sitten käyttää `.length`-ominaisuutta laskemaan haluamasi osan pituus. Tämä voi olla hyödyllistä esimerkiksi silloin, kun haluat tarkistaa onko merkkijonon alku- tai loppuosa tietty pituus.

## Katso myös

- [JavaScript String References](https://www.w3schools.com/jsref/jsref_obj_string.asp)
- [MDN Web Docs: String](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String)
- [Ecmascript.org: Strings](https://262.ecma-international.org/6.0/#sec-strings)