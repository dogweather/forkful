---
title:                "Att analysera HTML"
html_title:           "Javascript: Att analysera HTML"
simple_title:         "Att analysera HTML"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/parsing-html.md"
---

{{< edit_this_page >}}

# Varför
Att kunna parsra HTML är en viktig färdighet för utvecklare som arbetar med webbutveckling. Genom att kunna extrahera specifika delar av en webbsidas HTML-kod kan man manipulera och visa information på ett mer dynamiskt sätt.

# Så här gör du
```Javascript
const parser = new DOMParser();
const htmlString = "<html><body><h1>Hej världen!</h1></body></html>";
const htmlDoc = parser.parseFromString(htmlString, "text/html");
```
Det första steget för att kunna parsra HTML i Javascript är att skapa en DOMParser-instans. Sedan kan man ange en sträng med HTML-koden som ska parsras och det önskade formatet (texthtml) till `parser.parseFromString()`-metoden. Det returnerar ett dokumentobjekt som vi kan använda för att navigera i HTML-koden.

```Javascript
const text = htmlDoc.getElementsByTagName("h1")[0].textContent;
// text = "Hej världen!"
```
Genom att använda `getElementsByTagName()`-metoden kan vi få tag på alla element med ett visst HTML-elementnamn, i det här fallet "h1". Det returnerar en `HTMLCollection`, som är liknande en `array`, där vi kan hämta en specifik element med indexet. Sedan kan vi använda `textContent` för att hämta texten inuti elementet.

# Djupdykning
Det finns flera olika sätt att parsra HTML i Javascript, men det vanligaste är att använda DOMParser-klassen som nämndes i föregående avsnitt. Det går också att använda `innerHTML` för att hämta HTML-koden inuti ett element, men det är inte lika stabilt och kan lätt störa andra delar av koden. Det är därför rekommenderat att använda DOMParser för att vara mer specifik i vilken del av HTML-koden som ska parsras.

# Se även
- [DOMParser i Javascript](https://developer.mozilla.org/en-US/docs/Web/API/DOMParser)
- [HTMLCollection i Javascript](https://developer.mozilla.org/en-US/docs/Web/API/HTMLCollection)
- [index.js - ett verktyg för att parsra HTML i Node.js](https://github.com/isaacs/index.js)