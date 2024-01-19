---
title:                "Html:n jäsentäminen"
html_title:           "Javascript: Html:n jäsentäminen"
simple_title:         "Html:n jäsentäminen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/parsing-html.md"
---

{{< edit_this_page >}}

# HTML-Parsinnan Käyttö JavaScriptissä

## Mikä & Miksi?

Parsing HTML tarkoittaa HTML-tiedoston koodin jäsentämistä ja sen rakenteen analysointia. Ohjelmoijat tekevät tämän ymmärtääkseen paremmin HTML-sivun rakenteen, järjestääkseen HTML-elementtejä, tai muuttaakseen niiden sisältöä.

## Näin se toimii:
```Javascript
let parser = new DOMParser();
let doc = parser.parseFromString('<p>Tervetuloa!</p>', 'text/html');
console.log(doc.body.innerHTML);  // Tulostaa: <p>Tervetuloa!</p>
```
Esimerkkikoodimme käyttää DOMParser-objektia, joka luo uuden DOM-objektin merkkijonosta. Tämä DOM-objekti jäljittelee todellista HTML-sivua.

## Syvemmälle sukellus

HTML-parsintaan on käytetty useita eri tekniikoita historiallisesti, ja se on kehittynyt HTML:n ja JavaScriptin kehityksen mukana. Vanhemmissa JavaScript-versioissa saattoi nähdä "innerHTML"-ominaisuuden käyttöä, joka toimii yhä, mutta DOMParser on nykyisin suositeltu tapa.

Vaihtoehtoisesti ohjelmoijat voivat käyttää serveripuolen tekniikoita, kuten Node.js:ää ja 'cheerio' kirjastoa. Näitä käytetään yleensä suurten HTML-massojen käsittelyyn, tai tilanteissa, joissa ohjelmoijan täytyy käsitellä HTML:ää, jota hän ei varsinaisesti itse tuota.

DOMParserin käyttämisen yksityiskohta on, että sen "parseFromString" metodi palauttaa aina uuden "Document"-objektin. Tämä tarkoittaa, että voit aina luoda tarkkaan harkitun HTML-rakenteen, jota voit muokata ja käsitellä ilman, että se vaikuttaa alkuperäiseen HTML-sivuun.

## Katso myös

1. [DOMParser MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/API/DOMParser)
2. [HTML parsing algorithm in HTML5](https://html.spec.whatwg.org/multipage/parsing.html)
3. [Cheerio library Github page](https://github.com/cheeriojs/cheerio)