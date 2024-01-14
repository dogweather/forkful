---
title:                "TypeScript: Mallia vastaavien merkkien poistaminen"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi

Joskus ohjelmoinnissa tarvitaan tietyn kaavan mukaisten merkkien poistamista tekstimuodosta. Tämä voi esimerkiksi olla tarpeellista käyttäjän antaman syötteen validoinnissa tai datan käsittelyssä.

## Miten

Merkkien poistaminen halutun kaavan mukaan TypeScript-koodissa on helppoa. Käytämme siihen `replace()`-funktiota, joka ottaa vastaan kaksi parametria: ensimmäisenä kaava, joka kertoo mitkä merkit halutaan poistaa, ja toisena tyhjän merkkijonon, jolla nämä merkit korvataan. Katso esimerkki alla:

```TypeScript
let teksti = "Tämä on esimerkki 123 tekstistä!";
let kaava = /[0-9]/g; // poistetaan kaikki numerot
teksti = teksti.replace(kaava, ""); // teksti muuttuu nyt "Tämä on esimerkki tekstistä!"
console.log(teksti); // tulostaa: Tämä on esimerkki tekstistä!
```

Tässä esimerkissä olemme käyttäneet säännöllistä lausetta `[0-9]`, joka vastaa kaikkia numeroita 0-9. Käytämme myös `g` ("globaali") -modifieria, jotta kaikki vastaavat merkit poistetaan eikä vain ensimmäinen. 

## Syvempi sukellus

`replace()`-funktion lisäksi TypeScript tarjoaa muitakin tapoja poistaa merkkejä tekstistä. Jos haluat poistaa tiettyjä merkkejä alusta tai lopusta, voit käyttää `substring()`-metodia. Se ottaa vastaan kaksi parametria: ensimmäisenä indeksin, josta jäädään alkaen tekstiä, ja toisena indeksin, johon asti tekstiä poistetaan. Katso esimerkki alla:

```TypeScript
let teksti = "Tämä on esimerkki tekstistä!";
teksti = teksti.substring(5); // teksti muuttuu nyt "on esimerkki tekstistä!"
console.log(teksti); // tulostaa: on esimerkki tekstistä!
teksti = teksti.substring(0, 8); // teksti muuttuu nyt "on esime"
console.log(teksti); // tulostaa: on esime
```

Vaihtoehtoisesti voit myös käyttää `split()`-funktiota, joka jakaa tekstin annetun merkin kohdalta ja palauttaa siitä listan. Voit sitten yhdistää tämän listan takaisin yhdeksi merkkijonoksi `join()`-metodilla ja näin jättää tiettyjä merkkejä pois. Katso esimerkki alla:

```TypeScript
let teksti = "Tämä, on, esimerkki, tekstistä!";
teksti = teksti.split(", ").join(" "); // teksti muuttuu nyt "Tämä on esimerkki tekstistä!"
console.log(teksti); // tulostaa: Tämä on esimerkki tekstistä!
```

## Katso myös

- [MDN Web Docs - String.prototype.replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [MDN Web Docs - String.prototype.substring()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [MDN Web Docs - String.prototype.split()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/split)