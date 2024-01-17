---
title:                "Ladda ner en webbsida"
html_title:           "TypeScript: Ladda ner en webbsida"
simple_title:         "Ladda ner en webbsida"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att hämta en webbsida innebär att ladda ner all HTML, CSS och JavaScript-kod som utgör webbsidan från dess webbadress. Detta gör det möjligt för programmerare att manipulera och använda denna kod för att skapa dynamiska webbapplikationer och -tjänster.

## Hur gör man:
```TypeScript
import * as http from 'http';
const url = 'https://www.example.com';
http.get(url, (response) => {
    let data = '';
    response.on('data', (chunk) => {
        data += chunk;
    });
    response.on('end', () => {
        console.log(data);
    });
});
```
Detta exempel använder Node.js inbyggda modul "http" för att hämta en webbsida från adressen https://www.example.com. Kodblocket skapar en GET-förfrågan och tar emot svaret som strömmar in som "chunk"-ar. Därefter samlas all data ihop och loggas.

## Djupdykning:
Hämtning av webbsidor introducerades för att göra det möjligt att utveckla dynamiska webbplatser och applikationer, där innehållet kan ändras utan att sidan behöver laddas om. Det finns andra metoder för att hämta webbsidor såsom "fetch" API:t, men "http" modulen i Node.js är fortfarande en vanlig metod för att utföra denna uppgift.

## Se även:
- https://nodejs.org/api/http.html#http_http_get_options_callback för mer information om hur man hämtar webbsidor med "http" modulen.
- https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API för information om "fetch" API:t och dess möjligheter.