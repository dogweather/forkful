---
title:                "Merkkijonon pituuden löytäminen"
html_title:           "Javascript: Merkkijonon pituuden löytäminen"
simple_title:         "Merkkijonon pituuden löytäminen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi harrastaa merkkijonon pituuden löytämistä

Merkkijonojen käyttö on olennainen osa ohjelmointia ja niiden pituuden löytäminen on tärkeä taito jokaiselle Javascript-kehittäjälle. Merkkijonon pituuden löytäminen auttaa meitä esimerkiksi tarkistamaan käyttäjän antaman syötteen pituuden tai muokkaamaan merkkijonoja haluamallamme tavalla. Se on myös osa perustason ohjelmointitehtäviä, joten sen osaaminen on tärkeää.

## Miten löytää merkkijonon pituus

Merkkijonon pituuden löytämiseksi voimme käyttää Javascriptin `length`-ominaisuutta, joka palauttaa merkkijonon pituuden numerona. Alla on esimerkki käytöstä:

```Javascript
// Luo muuttuja ja tallenna siihen merkkijono
let merkkijono = "Tämä on esimerkki";

// Tulosta merkkijonon pituus käyttäen length-ominaisuutta ja console.log() -funktiota
console.log(merkkijono.length); // Tulostaa 18
```

Merkkijonon pituuden lisäksi voimme myös käyttää `charAt()`-funktiota, joka palauttaa merkkijonon tietyn indeksin mukaisen merkin. Tätä funktiota hyödyntäen voimme luoda oman funktion merkkijonon pituuden löytämiseksi. Alla on esimerkkikoodi:

```Javascript
// Luo funktion, joka laskee annetun merkkijonon pituuden
function laskePituus(merkkijono) {
  let pituus = 0; // Alustetaan muuttuja arvolla 0

  // Käy läpi merkkijono ja lisää pituuteen yksi jokaisen merkin kohdalla
  for (let i = 0; i < merkkijono.length; i++) {
    pituus += 1;
  }
  return pituus; // Palauta pituus muuttujan arvo
}

// Testaa funktion toimintaa
let testi = "Toinen esimerkki";
console.log(laskePituus(testi)); // Tulostaa 17
```

## Syventävä tieto merkkijonon pituudesta

Vaikka Javascriptin `length`-ominaisuus on helppo tapa löytää merkkijonon pituus, on hyvä ymmärtää, miksi se toimii ja miten se laskee pituuden. Merkkijonot ovat tietorakenteita, joissa jokainen merkki vie yhden muistipaikan. `length`-ominaisuus laskee merkkijonon muistipaikkojen määrän ja palauttaa sen numerona, jolloin saamme merkkijonon pituuden.

## Katso myös

- [MDN JavaScript String length](https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [W3Schools JavaScript String Length](https://www.w3schools.com/jsref/jsref_length_string.asp)