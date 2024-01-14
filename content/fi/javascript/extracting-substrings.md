---
title:    "Javascript: Alaryhmien eristäminen"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Miksi: Miksi sinun pitäisi käyttää JavaScript-koodia alirivejä mahdollistavia substringeja?

Oletko koskaan joutunut työskentelemään suurten tekstien kanssa? Onko sinun tarvinnut etsiä tiettyä osaa merkkijonosta? Näinä tilanteina substringit voivat olla suureksi avuksi. Ne mahdollistavat tietyn osan merkkijonon erottamisen ja käytön halutussa muodossa.

## Miten: Esimerkkejä ja koodipätkiä

```Javascript
// Alirivin erottaminen käyttäen substr() -funktiota
var sana = "Tervetuloa";
var alirivi = sana.substr(6, 3);
console.log(alirivi);
// Tulostaa: "ulo"

// Alirivin erottaminen ja muuttaminen isolla kirjaimella käyttäen toUpperCase() -funktiota
var lause = "Koodataan yhdessä";
var alirivi = lause.substr(9, 6).toUpperCase();
console.log(alirivi);
// Tulostaa: "YHDESSÄ"
```

Substringit toimivat myös muiden merkkijonomuotojen kanssa, kuten esimerkiksi taulukoissa ja muuttujissa.

## Syvempi sukellus: Tietoa alirivien erottamisesta

Substringit on mahdollista erottaa kahdella eri tavalla: substr() ja substring(). Ensimmäinen tapa erottaa osa merkkijonoa alkaen annetusta indeksistä ja jatkuen annetun määrän verran. Toisessa tavassa annetaan alku- ja loppuindeksit, ja se erottaa merkkijonon niiden väliltä.

On myös tärkeää huomata, että merkkijonojen indeksointi alkaa aina nollasta. Tämä tarkoittaa sitä, että esimerkiksi merkkijonossa "Tervetuloa" indeksit 0-7 vastaavat kirjaimia "T", "e", "r", "v", "e", "t", "u", "l", "o" ja indeksit 7-10 vastaavat "a", "space" ja "E". Hyödyllistä tietoa, kun haluaa erottaa tietyn pituisen alirivin!

## Katso myös

- [MDN web docs - substr()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substr)
- [MDN web docs - substring()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [W3Schools - JavaScript Substrings](https://www.w3schools.com/js/js_string_substrings.asp)

Kiitos lukemisesta! Toivottavasti tästä oli apua alirivien erottamisessa ja koodisi tekee siitä tulevaisuudessa entistä helpompaa. Onnea projektien ja tekstien käsittelyssä!