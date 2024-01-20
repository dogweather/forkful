---
title:                "Alimerkkijonojen poiminta"
html_title:           "Gleam: Alimerkkijonojen poiminta"
simple_title:         "Alimerkkijonojen poiminta"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Alakielten erottaminen on ohjelmoinnin tekniikka, jossa tietyt osat merkkijonosta valitaan ja kopioidaan uuden merkkijonon muodostamiseksi. Ohjelmoijat tekevät tämän tiedon käsittelyyn, kuten tekstianalyysiin tai syötteen jäsentämiseen.

## Miten:

Voit poimia alamerkkijonoja TypeScriptissä käyttämällä `substring()`, `substr()`, tai `slice()` metodeja. Tässä on esimerkkejä siitä, miten näitä metodeja voidaan käyttää:

```TypeScript
let str = "Hei, TypeScript!"

// Käyttämällä substring-menetelmää
let substring = str.substring(4, 14);
console.log(substring); // Tulostaa: TypeScript

// Käyttämällä substr-menetelmää
let substr = str.substr(4, 10);
console.log(substr); // Tulostaa: TypeScript

// Käyttämällä slice-menetelmää
let sliced = str.slice(4, 14);
console.log(sliced); // Tulostaa: TypeScript
```

## Syvä sukellus:

Historiallisesti `substr()` funktio oli ensimmäinen tarjolla oleva merkkijonojen käsittelymetodi ja se on peräisin JavaScriptestä. Myöhemmin lisättiin `substring()` ja `slice()`, jotka tarjoavat enemmän joustavuutta ja ovat selkeämmät käyttää pituuden ja indeksin kanssa.

Mitä tulee vaihtoehtoihin, voit käyttää säännöllisiä lausekkeita tai luoda oman toimintosi, jos nämä menetelmät eivät täytä tarpeitasi.

Mitä tulee toiminnan yksityiskohtiin, sekä `substring()` että `slice()` toimivat samalla tavalla, ellei niille anneta negatiivista arvoa indeksiksi. Tässä tapauksessa `slice()` ottaa jäljellä olevat merkit merkkijonosta, kun taas `substring()` kohtelee negatiivisen arvon nollana.

## Katso myös:

1. MDN Web Docs, merkki¬jono¬metodit: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String
2. TypeScript käsikirja, merkkijonot: https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#strings
3. StackOverflow, merkkijonojen käsittely TypeScriptissä: https://stackoverflow.com/questions/tagged/typescript+strings