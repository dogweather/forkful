---
title:                "TypeScript: Ladda ner en webbsida"
simple_title:         "Ladda ner en webbsida"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Varför

Det finns många olika skäl till varför någon skulle vilja ladda ner en webbsida. Det kan vara för att spara information, utveckla en egen webbapplikation eller bara för enkel åtkomst när du inte är uppkopplad.

## Så här

För att ladda ner en webbsida med TypeScript använder du dig av biblioteket "node-fetch". Detta gör det möjligt att göra HTTP-förfrågningar från din TypeScript-kod. Börja med att installera biblioteket med kommandot:

```
npm install node-fetch
```

Sedan kan du använda följande kod för att ladda ner en webbsida och spara den som en textfil:

```
import fetch from 'node-fetch';

async function downloadWebpage(url: string, fileName: string) {
  // hämta webbsidan
  const response = await fetch(url);
  // hämta innehållet som text
  const text = await response.text();
  // spara texten i en textfil
  fs.writeFileSync(fileName, text);
}

// anropa funktionen och ange URL och filnamn
downloadWebpage("https://www.example.com", "example.txt");
```

När koden har kört kommer det finnas en fil med namnet "example.txt" i samma mapp som din TypeScript-fil.

## Djupdykning

När du laddar ner en webbsida med TypeScript är det viktigt att förstå hur HTTP-förfrågningar fungerar. En HTTP-förfrågan är en begäran som skickas från en klient (t.ex. en webbläsare eller ett program) till en server. För att ladda ner en webbsida behöver du skicka en GET-förfrågan till rätt URL och sedan hantera det svar som servern skickar tillbaka.

När du använder "node-fetch" biblioteket skapas ett "Response" objekt som innehåller information om förfrågan och det svar som servern skickade. I det här fallet använder vi metoden "text()" för att hämta innehållet på webbsidan som en textsträng. Detta gör det möjligt att spara innehållet på webbsidan som en textfil.

## Se även

- [node-fetch dokumentation](https://www.npmjs.com/package/node-fetch)
- [En guide till TypeScript](https://www.typescriptlang.org/docs/)