---
title:                "Ladda ner en webbsida"
html_title:           "Bash: Ladda ner en webbsida"
simple_title:         "Ladda ner en webbsida"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Ladda ner en webbsida innebär att hämta dess kompletta innehåll i kod. Programmerare gör det för att analysera strukturen, optimera prestanda eller för att skrapa data.

## Hur man gör:
Här är ett enkelt exempel på att ladda ner en webbsida i JavaScript med `node-fetch` biblioteket.

```
Javascript
const fetch = require('node-fetch');

let url = "http://example.com";

fetch(url)
  .then(response => response.text())
  .then(data => console.log(data));
  ```
Ovenstående kod laddar ner webbsidan på den angivna URL:en och skriver ut innehållet i konsolen.

## Djupdykning
Tidigare användes metoder som 'wget' eller 'curl' för att ladda ner webbsidor, men nu görs det vanligtvis med olika JavaScript-bibliotek, som 'axios' eller 'node-fetch'. 'node-fetch' är baserad på Window.Fetch API och är ofta använd för sömlös server till server kommunikation. Alternativt kan 'puppeteer' biblioteket användas för en mera fullständig webbbläsare upplevelse, inklusive JavaScript exekvering.

Detaljer: Jadå, allt detta kan låta enkelt men vi måste hantera olika situationer. Som att säkert hantera 404 fel och timeout situationer. Också, det kan vara viktigt för oss att exekvera JavaScript på den laddade sidan, vilket 'node-fetch' inte hjälper oss med, men 'puppeteer' klarar däremot av.

## Se Även:
För mer information och tutorials, se dessa länkar:
- Node Fetch: https://github.com/node-fetch/node-fetch
- Axios dokumentation: https://github.com/axios/axios
- Puppeteer dokumentation: https://pptr.dev/
- Window.Fetch API: https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API
- JavaScript exekvering med Puppeteer: https://javascript.info/puppeteer