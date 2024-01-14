---
title:    "Javascript: Merkkijonon muuttaminen pieniksi kirjaimiksi"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit muuttaa merkkijonon pieniksi kirjaimiksi? On olemassa useita syitä, miksi tämä voi olla hyödyllistä ohjelmointitehtävissä. Se voi auttaa vertaamaan kahta merkkijonoa, kun kirjainsuuruudella ei ole merkitystä. Se myös helpottaa merkkijonon hakemista ja muokkaamista, kun se on yhtenäisessä muodossa.

## Miten

Voit muuttaa merkkijonon pieniksi kirjaimiksi käyttämällä Javascriptin `toLowerCase()`-funktiota. Seuraavassa esimerkissä oletetaan, että meillä on merkkijono "TÄMÄ ON ESIMERKKI":

```javascript
let string = "TÄMÄ ON ESIMERKKI";
let lowerCase = string.toLowerCase();
console.log(lowerCase); // tulostaa "tämä on esimerkki"
```

## Syvällinen sukellus

Käyttämällä `toLowerCase()`-funktiota, Javascript muuttaa jokaisen merkin merkkijonossa vastaavaksi pieneksi kirjaimeksi. Tämä toimii vain ASCII-merkkijonoille, eli esimerkiksi ääkköset eivät välttämättä muutu. Jos haluat käsitellä kansainvälisiä merkkejä, voit käyttää `toLocaleLowerCase()`-funktiota, joka tukee Unicode-merkkejä.

On myös hyvä muistaa, että `toLowerCase()` ei muuta alkuperäistä merkkijonoa, vaan palauttaa uuden muutetun merkkijonon. Voit tallentaa tämän uuden arvon esimerkiksi uuteen muuttujaan tai sijoittaa sen suoraan haluamaasi muuttujaan.

## Katso myös

- [toLowerCase() dokumentaatio (MDN)](https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [Kansainvälisten merkkien käsittely Javascriptissä (W3Schools)](https://www.w3schools.com/JSREF/jsref_tolocalelowercase.asp)