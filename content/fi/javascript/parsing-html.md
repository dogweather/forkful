---
title:                "HTML:n jäsentäminen"
date:                  2024-01-20T15:32:05.370124-07:00
html_title:           "Bash: HTML:n jäsentäminen"
simple_title:         "HTML:n jäsentäminen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?
HTML-parsiminen tarkoittaa HTML-dokumentin rakenteen muuntamista joksikin, mitä ohjelma voi käsitellä. Koodarit tekevät tätä sisällön kaivamiseksi tai dokumentin manipuloimiseksi.

## How to:
```Javascript
const parser = new DOMParser();
const htmlString = "<!DOCTYPE html><p>Hello, world!</p>";
const doc = parser.parseFromString(htmlString, "text/html");

console.log(doc.body.textContent); // Output: Hello, world!
```

## Deep Dive:
HTML-parsiminen on vanha juttu, se on kulkenut käsikädessä webin kehityksen kanssa. Vanhemmiten syntyi DOM (Document Object Model), joka standardisoi, miten dokumentteja käsitellään. Vaihtoehtoja on, kuten `innerHTML` ja kirjastoja kuten jQuery tai modernimmat React ja Vue.js. DOMParser käyttää web-selaimen sisäistä parsijaa, joten se on yhdenmukainen ja turvallinen.

## See Also:
- MDN Web Docs DOMParser: https://developer.mozilla.org/en-US/docs/Web/API/DOMParser
- HTML Living Standard: https://html.spec.whatwg.org/multipage/parsing.html
- jQuery.parseHTML(): https://api.jquery.com/jQuery.parseHTML/
