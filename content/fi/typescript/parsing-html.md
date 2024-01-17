---
title:                "Html-analysointi"
html_title:           "TypeScript: Html-analysointi"
simple_title:         "Html-analysointi"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/parsing-html.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
HTML:n jäsentäminen on prosessi, jossa tietokone lukee ja ymmärtää HTML-koodia ja muuttaa sen rakenteeksi, jota voidaan käsitellä ja näyttää selaimessa. Tämä on tärkeää, koska se mahdollistaa dynaamisten ja monipuolisten verkkosivujen luomisen, joilla on monia erilaisia elementtejä ja sisältöä.

## Kuinka tehdä:
Seuraavissa koodiesimerkeissä näet, kuinka voit jäsentää HTML-koodia TypeScript-ohjelmointikielellä ja saada tulosteen, joka näyttää rakenteen ja sisällön.

```TypeScript
const html = "<div><h1>Tervetuloa!</h1><p>Tervetuloa sivullemme!</p></div>";
const parser = new DOMParser();
const doc = parser.parseFromString(html, "text/html");
console.log(doc);
```

Tulostus:

```
#document
<html>
  <head></head>
  <body>
    <div>
      <h1>Tervetuloa!</h1>
      <p>Tervetuloa sivullemme!</p>
    </div>
  </body>
</html>
```

## Syvempi sukellus:
HTML:n jäsentäminen on kehittynyt yhdessä verkon kehityksen kanssa ja on olennainen osa modernia verkkokehitystä. On myös muita tapoja jäsentää HTML-koodia, kuten käyttämällä Regular Expression -lausekkeita, mutta DOM-hierarkian käyttäminen on yleisesti suositeltavaa. HTML-jäsennys suoritetaan yleensä selaimen sisällä, mutta se voidaan myös tehdä palvelimella ennen kuin sivu lähetetään selaimelle. Tämä voi parantaa verkkosivujen latausnopeutta ja suorituskykyä.

## Tutustu myös:
- [MDN Web Docs: HTML-parsing](https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model/Introduction)
- [Cheatsheet: TypeScript HTML parsing](https://dev.to/andresfedeli/javascript-parse-html-to-json-objects-4d1l)
- [GitHub: TypeScript-DOM-parser](https://github.com/Cyb3rrJack/TypeScript-DOM-parser)