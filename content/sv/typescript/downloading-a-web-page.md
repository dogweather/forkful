---
title:                "Ladda ner en webbsida"
html_title:           "Bash: Ladda ner en webbsida"
simple_title:         "Ladda ner en webbsida"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att ladda ner en webbsida innebär att extrahera dess innehåll för offlineanvändning eller dataanalys. Programmerare gör detta för att bearbeta stora mängder webbinformation för automatiserade system.

## Så här gör du:
Här kommer vi att använda 'axios' och 'cheerio' för att ladda ner och bearbeta webbsidan. Installera paketen med `npm install axios cheerio`.

```TypeScript
import axios from 'axios';
import cheerio from 'cheerio';

async function laddaNerSida(url: string) {
    const response = await axios.get(url);
    const $ = cheerio.load(response.data);
    // bearbeta webbsidan med cheerio
    console.log($('body').text());
}

laddaNerSida('https://www.example.com');
```

Kör koden och se utdata:

```
> node index.ts
Hem | Exempel Domän
...
```

## Djupdykning
Att ladda ner webbsidor har använts sedan slutet av 1990-talet för att mata webbsökrobotar och databaser. Alternativ inkluderar användning av webbläsarens API: er eller webbskrapningsramar som Puppeteer.

Implementeringsdetaljerna inkluderar att skicka en GET-förfrågan till webbservrar, tolka svaret och konvertera HTML-data till ett manipulerbart format. Cheerio paketet används för att leverera jQuery-liknande metoder för att bearbeta och navigera i HTML-strukturen.

## Se även
- [Axios - Promisbaserade HTTP-begäran](https://www.npmjs.com/package/axios)
- [Cheerio - Snabb, flexibel och funktionsrik implementation av kärn-jQuery](https://www.npmjs.com/package/cheerio)
- [Puppeteer - Headless Chrome/Noder.js API](https://pptr.dev/)