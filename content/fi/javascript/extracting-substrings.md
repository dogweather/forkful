---
title:                "Alimerkkijonojen poiminta"
html_title:           "Gleam: Alimerkkijonojen poiminta"
simple_title:         "Alimerkkijonojen poiminta"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Merkkijonoista osamerkkijonojen (substring) poimiminen on prosessi, jossa otetaan osa kokonaisesta merkkijonosta. Tämä on hyödyllistä useissa tilanteissa, kuten datan käsittelyssä ja tiettyjen koodirakenteiden parantamisessa.

## Kuinka:

Katsotaanko esimerkki Javascriptin 'substring', 'substr' ja 'slice' metodeista.

```Javascript
let str = "Ohjelmointi on kivaa";

// Kaikki alla olevat esimerkit tulostavat "Ohjelma"
console.log(str.substring(0,7));
console.log(str.substr(0,7));
console.log(str.slice(0,7));
```

Ero näiden välillä on se, miten ne käsittelevät syöttöparametreja. Esimerkiksi, jos negatiivinen arvo annetaan toiseksi parametriksi substr-metodille, se otetaan merkkijonon pituudesta:

```Javascript
console.log(str.substr(0,-2)); // tulostaa "Ohjelmointi on k"
```

## Syvempi sukellus:

Alunperin, JavaScriptissä oli vain 'substring' metodi. 'substr' ja 'slice' lisättiin myöhemmin tarjoamaan enemmän joustavuutta.

- 'substring' ottaa kaksi argumenttia: aloitusindeksin ja lopetusindeksin. 
- 'substr' ottaa aloitusindeksin ja palautettavan merkkijonojen määrän.
- 'slice' toimii kuten 'substring', mutta se sallii negatiiviset indeksit, jotka viittaavat merkkijonon loppuun.

On tärkeää tunnistaa nämä erot ja valita oikea metodi tarpeidesi mukaan.

## Katso myös:

Lisätietoja saadaksesi, tutustu seuraaviin lähteisiin:

- MDN Web Docs ja sen artikkeli ["String.prototype.substring()"](https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- W3Schoolsin opas ["JavaScript String Methods"](https://www.w3schools.com/js/js_string_methods.asp)