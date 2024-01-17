---
title:                "Analysera html"
html_title:           "TypeScript: Analysera html"
simple_title:         "Analysera html"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/parsing-html.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Parsing HTML (HTML-analys) är en process där man tar en HTML-kod och omvandlar den till en trädstruktur, vilket gör det möjligt att enklare manipulera och visa informationen från koden. Detta är en vanlig uppgift för webbutvecklare, eftersom det tillåter dem att skapa dynamiska webbsidor med hjälp av JavaScript.

## Hur man gör:

```TypeScript
const html = "<h1>Hello World</h1><p>This is a paragraph</p>";

// Skapa en HTML-analysator
const parser = new DOMParser();

// Konvertera HTML-koden till ett träd
const doc = parser.parseFromString(html, "text/html");

// Hämta alla element med taggen "p"
const paragraphs = doc.getElementsByTagName("p");

// Visa innehållet av det första elementet
console.log(paragraphs[0].textContent);

/* Output:
   This is a paragraph
*/
```

## Djupdykning:

HTML parsering har funnits sedan början av webbens utveckling för att göra processen med att visa webbsidor mer effektiv. Det finns flera olika alternativ för att utföra HTML parsering, inklusive inbyggda funktioner i webbläsare och externa bibliotek. I TypeScript kan man använda DOMParser för att analysera HTML och sedan använda DOM-trädet för att manipulera eller visa elementen på en webbsida.

## Se även:

- [DOMParser dokumentation](https://developer.mozilla.org/en-US/docs/Web/API/DOMParser)
- [Bärklara HTML parser](https://github.com/taoqf/node-html-parser)
- [Implementering av HTML parser i TypeScript](https://github.com/Microsoft/TSJS-lib-generator/blob/master/src/lib/dom.generated.d.ts#L10163)