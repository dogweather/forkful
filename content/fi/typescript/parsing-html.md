---
title:                "HTML:n jäsentäminen"
date:                  2024-01-20T15:34:31.691443-07:00
html_title:           "Bash: HTML:n jäsentäminen"
simple_title:         "HTML:n jäsentäminen"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/parsing-html.md"
---

{{< edit_this_page >}}

## What and Why? - Mikä ja Miksi?
HTML:n jäsentäminen tarkoittaa HTML-merkkijonon muuttamista rakenteelliseen muotoon, kuten DOM-puuksi. Ohjelmoijat tekevät tämän datan kaivamiseksi, dokumenttien muokkaukseksi tai web-sisällön skräppäämiseksi.

## How to: - Miten:
```TypeScript
import * as parser from 'node-html-parser';

const html = '<!DOCTYPE html><html><body><h1>Otsikko 1</h1><p>Ensimmäinen kappale.</p></body></html>';
const root = parser.parse(html);

// Etsi elementtejä
const h1Text = root.querySelector('h1').textContent;
console.log(h1Text); // Output: Otsikko 1

const pText = root.querySelector('p').textContent;
console.log(pText); // Output: Ensimmäinen kappale.

// Muokkaa HTML:ää
root.querySelector('h1').set_content('Muutettu Otsikko');
console.log(root.toString()); // Output: muuttaa <h1>:n sisällön
```

## Deep Dive - Syvä Sukellus:
Historiallisesti HTML:n jäsentäminen JavaScriptillä tapahtui pääasiassa selaimessa, jossa DOM API teki työn. Noden myötä tarve palvelinpuolen HTML-jäsennykselle kasvoi. Tämä johti kirjastoihin kuten `node-html-parser`.

Vaihtoehtoisia työkaluja on lukuisia, kuten Cheerio ja JSDOM. Cheerio on nopea ja kevyt, kun taas JSDOM pyrkii jäljittelemään selaimen ympäristöä tarkemmin.

Implementoinnissa on tärkeää pitää mielessä, että HTML-dokumentit voivat olla epäjohdonmukaisia. Jäsentäjän täytyy olla joustava ja toleroida virheitä.

## See Also - Katso Myös:
- [DOMstandardi](https://dom.spec.whatwg.org/): Yksityiskohtainen selostus siitä, miten DOM toimii.
- [node-html-parser](https://www.npmjs.com/package/node-html-parser): Jäsennyskirjasto, jota käytimme esimerkeissä.
- [Cheerio](https://www.npmjs.com/package/cheerio): jQuery-tyylinen kirjasto HTML:n käsittelyyn Node.js:ssä.
- [JSDOM](https://www.npmjs.com/package/jsdom): Node.js-ympäristölle tehty selainmalli.
