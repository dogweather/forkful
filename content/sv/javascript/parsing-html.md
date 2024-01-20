---
title:                "Tolka HTML"
date:                  2024-01-20T15:32:27.921799-07:00
html_title:           "Arduino: Tolka HTML"
simple_title:         "Tolka HTML"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/parsing-html.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att parsa HTML innebär att omvandla den till något som JavaScript kan förstå och manipulera. Detta görs för att kunna extrahera data, manipulera innehåll eller för att automatiskt generera dokumentstruktur.

## Hur gör man?
Du kan använda `DOMParser` för att parsa en sträng av HTML. Här är ett enkelt exempel:

```Javascript
let parser = new DOMParser();
let doc = parser.parseFromString('<p>Hej Sverige!</p>', 'text/html');
console.log(doc.body.textContent); // Skriver ut "Hej Sverige!"
```

Sample output:
```
Hej Sverige!
```

För att interagera med de parsade elementen använd `querySelector` eller `getElementById`:

```Javascript
let pElement = doc.querySelector('p');
console.log(pElement.textContent); // Skriver ut "Hej Sverige!"
```

## Fördjupning
Historiskt sett har webb-utvecklare använt olika tekniker för att parsa HTML, inklusive reguljära uttryck och tredjepartsbibliotek som jQuery. Men `DOMParser` är en webbstandard, vilket gör den till en pålitlig och konsistent lösning.

Alternativ till `DOMParser` inkluderar `innerHTML` och olika bibliotek som Cheerio (för Node.js) eller Beautiful Soup (för Python). Dessa är bra när `DOMParser` inte är tillräckligt, som vid server-side rendering eller när du jobbar i miljöer utan en DOM.

Implementation av HTML-parsing handlar oftast om effektivitet och säkerhet. Se till att den HTML du parsa inte innehåller skadlig kod, särskilt vid användning av `innerHTML`.

## Se även
- MDN Web Docs om `DOMParser`: https://developer.mozilla.org/en-US/docs/Web/API/DOMParser
- CheerioJS documentation: https://cheerio.js.org/
- Beautiful Soup dokumentation (för Python utvecklare): https://www.crummy.com/software/BeautifulSoup/