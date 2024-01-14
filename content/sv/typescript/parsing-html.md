---
title:                "TypeScript: Att parsa HTML"
simple_title:         "Att parsa HTML"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/parsing-html.md"
---

{{< edit_this_page >}}

## Varför

Att parsning av HTML är en vanlig uppgift för många webbutvecklare, eftersom det är en grundläggande del av att bygga dynamiska webbplatser. Genom att lära sig hur man effektivt kan parse HTML, kan man göra webbutveckling snabbare och enklare.

## Hur man gör

För att börja parsning av HTML i TypeScript behöver du först installera ett externt bibliotek som heter "cheerio". Detta bibliotek gör det möjligt för oss att selektera och manipulera HTML-element.

När du har installerat "cheerio", kan du använda följande kod för att skapa en enkel HTML-parsfunktion:

```TypeScript
import * as cheerio from 'cheerio';

function parseHTML(html: string): string[] {
    const $ = cheerio.load(html);
    const elements = $('p'); // här väljer vi att söka efter alla <p>-element
    const output: string[] = [];
    
    elements.each((index, element) => {
        output.push($(element).text()); // här hämtar vi texten från varje <p>-element och lägger till den i vår utmatnings-array
    });
    
    return output;
}

const html = '<body><h1>Välkommen</h1><p>Det här är en text</p><p>En annan text</p></body>';
const parsed = parseHTML(html);

console.log(parsed); // ["Det här är en text", "En annan text"]
```

I det här exemplet skapar vi en funktion som tar emot en HTML-sträng och returnerar en array med all text som finns i <p>-elementen i HTML:n. Vi använder oss av cheerio-biblioteket för att enkelt välja ut alla <p>-element och hämta deras text.

Som du kan se är det väldigt enkelt att parsra HTML med hjälp av TypeScript. Det är bara att använda selektor-syntaxen för att välja ut de önskade elementen och sedan använda .text() metoden för att hämta texten från dem.

## Djupdykning

När vi parsar HTML är det viktigt att förstå hur DOM-trädet fungerar. DOM-trädet är en hierarkisk struktur som representerar alla element och deras förhållanden till varandra i en HTML-sida. När vi använder cheerio-biblioteket, läser vi i princip in HTML:n och bygger upp en miniatyrversion av DOM-trädet i minnet.

För att kunna selektera och manipulera element måste vi förstå de olika selektorerna som finns tillgängliga i cheerio. De är baserade på CSS-selektorer, så om du är bekant med CSS kommer du snabbt lära dig hur man använder dem inom cheerio.

Det är också bra att veta att cheerio har stöd för både jQuery-syntax och vanlig HTML-syntax. Det ger oss möjlighet att använda olika sätt att välja och manipulera element på, beroende på vilket som känns mest naturligt för just oss.

## Se även

- [Cheerio on GitHub](https://github.com/cheeriojs/cheerio)
- [jQuery Selectors](https://www.w3schools.com/jquery/jquery_selectors.asp)
- [DOM Tree Explanation](https://www.w3schools.com/js/js_htmldom.asp)