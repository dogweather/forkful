---
title:    "Javascript: Merkkijonojen yhdistäminen"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi

Useimmissa tapauksissa ohjelmoijat joutuvat tekemään toimintoja, joissa useita merkkijonoja täytyy yhdistää yhdeksi suuremmaksi merkkijonoksi. Tätä kutsutaan merkkijonojen yhdistämiseksi tai concatenationiksi. Tämä on tärkeä osa ohjelmoinnin perusteita, ja se säästää aikaa ja vaivaa, kun työskentelet monipuolisilla merkkijonoilla.

## Miten

Merkkijonojen yhdistäminen on helppoa JavaScriptissä. Voit käyttää "+" -merkkiä tai "concat ()" -metodia. Alla on esimerkki käyttämällä näitä kahta tapaa:

```Javascript
// Käyttäen "+" merkkiä
let etunimi = "Matti";
let sukunimi = "Meikäläinen";
let kokoNimi = etunimi + " " + sukunimi;
console.log(kokoNimi); // Tulostaa: Matti Meikäläinen

// Käyttäen "concat()" metodia
let aineisto1 = "Esimerkki";
let aineisto2 = "teksti";
let uusiAineisto = aineisto1.concat(" ", aineisto2);
console.log(uusiAineisto); // Tulostaa: Esimerkki teksti
```

## Syvempi sukellus

JavaScriptissä merkkijonot ovat ei-muuttuvia arvoja eli niitä ei voi muuttaa suoraan. Siksi jokainen merkkijonon yhdistämistä koskeva toiminto luo aina uuden merkkijonon. Tämä tarkoittaa sitä, että jokainen yhdistetty merkkijono vie muistitilaa, joten on tärkeää käyttää tätä toimintoa harkiten.

On myös huomattava, että merkkijonat eivät ole ainoita asioita, jotka voidaan yhdistää. Voit myös yhdistää muita tietotyyppejä, kuten numeroita ja muuttujia, merkkijonon kanssa. Käyttämällä "toString ()" -metodia voit muuttaa numeron merkkijonoksi, jotta se voidaan yhdistää toiseen merkkijonoon.

## Katso myös

- [Merkkijonojen yhdistäminen JavaScriptissä (MDN)](https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
- [JavaScriptin perusteet (W3Schools)](https://www.w3schools.com/js/)
- [JavaScript merkkijonot (Codecademy)](https://www.codecademy.com/learn/introduction-to-javascript/modules/introduction-to-javascript/cheatsheet)