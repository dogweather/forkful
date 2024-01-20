---
title:                "HTML:n jäsentäminen"
html_title:           "Bash: HTML:n jäsentäminen"
simple_title:         "HTML:n jäsentäminen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/parsing-html.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Parsing (jäsennys) HTML:ssa tarkoittaa HTML-dokumentin rakenteen muuntamista sellaiseen muotoon, josta ohjelmamme voi helposti lukea tiedot. Jäsentämistä käytetään usein verkkosivujen tiedonlouhintaan ja sen tuottamiseen muissa muodoissa.

## Kuinka:

HTML-jääsentämiseen JavaScriptissa voit käyttää DOMParser- tai BeautifulSoup4-kirjastoa, esimerkkinä näytetään DOMParser.

```Javascript
let htmlString = '<ul><li class="item">Item 1</li><li class="item">Item 2</li></ul>';
let domParser = new DOMParser();
let docNode = domParser.parseFromString(htmlString, "text/html");
let listItems = docNode.querySelectorAll('.item');

console.log(listItems[0].textContent); // Outputs: Item 1
console.log(listItems[1].textContent); // Outputs: Item 2
```

## Deep Dive:
Historia: HTML-jääsentäminen on ollut tärkeä osa web-scrappingia JavaScriptin alkuajoista lähtien. Jokaiselle verkkosivulle muodostuvan DOM-puun analysaaminen auttaa meitä ymmärtämään sivun rakennetta ja kaivamaan tarvittavat tiedot.

Vaihtoehdot: DOMParserin lisäksi on paljon muita vaihtoehtoja, kuten JQuery tai Node.js:ään perustuva jsdom, jotka tarjoavat samanlaisten tulosten hakemisen.

Toteutustiedot: Jäsennintyökalut muuntavat HTML-koodin DOM-puuksi, joka koostuu solmuista ja objekteista. Tämä muunto mahdollistaa sivun rakenteen tutkimisen ja erityisten elementtien poimimisen.

## Katso Myös:

1. [JavaScript DOM Parser - MDN Web docs](https://developer.mozilla.org/en-US/docs/Web/API/DOMParser)
2. [JQuery - Parsing HTML](https://jquery.com/)
3. [Node.js HTML parsing - jsdom](https://github.com/jsdom/jsdom)