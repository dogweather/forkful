---
title:                "Analysera html"
html_title:           "Arduino: Analysera html"
simple_title:         "Analysera html"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/parsing-html.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att tolka (parse) HTML innebär att omvandla HTML-kod, vilket är text, till strukturerade data. Programmerare gör detta för att kunna interagera med webbsidor, analysera strukturen, manipulera innehållet och extrahera data.

## Så här gör du:

Med hjälp av JavaScripts inbyggda `DOMParser` kan vi enkelt tolka (parse) HTML. Se följande exempel:

```Javascript
// Skapa ett nytt DOMParser objekt
let parser = new DOMParser();

// HTML-text som ska tolkas
let htmlString = "<ul><li>Exempel 1</li><li>Exempel 2</li></ul>";

// Använd parseFromString metoden
let doc = parser.parseFromString(htmlString, "text/html");

console.log(doc.body);
```

Efter att ha kört koden kommer outputten se ut som följer:

```Javascript
// Output
<ul><li>Exempel 1</li><li>Exempel 2</li></ul>
```

## Djupdykning:

Att tolka HTML går tillbaka till tidigt 90-tal där det ursprungligen utvecklades för att strukturera och tolka webbinnehåll. Det finns andra alternativ för att tolka HTML, till exempel `jquery's $() funktion` och `cheerio library` för Node.js, men DOMParser anses mer robust och säkrare då det inte utför script-taggar.

När det gäller implementation, så skapar DOMParser ett nytt `Document` objekt. Metoden `parseFromString` tolkar HTML-string och returnerar sedan detta objekt för vidare interaktion eller manipulation.

## Se även:

För ytterligare information, refererar jag till följande källor:
1. [MDN Web Docs - DOMParser](https://developer.mozilla.org/en-US/docs/Web/API/DOMParser)
3. [Stack Overflow - Parse HTML string with JavaScript](https://stackoverflow.com/questions/3103962/converting-html-string-into-dom-elements)