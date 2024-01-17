---
title:                "Skicka en http-begäran"
html_title:           "TypeScript: Skicka en http-begäran"
simple_title:         "Skicka en http-begäran"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skicka en HTTP förfrågan är när en programmerare skickar en begäran till en webbsida eller server för att hämta information. Programmers skickar HTTP förfrågningar för att få tillgång till data från en extern källa, som en API.

## Hur To:
Ett vanligt sätt att skicka en HTTP begäran i TypeScript är att använda det inbyggda fetch-API:et. Koden nedan visar ett exempel på hur man hämtar data från en extern server och loggar svaret till konsolen.

```TypeScript
    fetch("https://example.com/api/data") //Skicka en HTTP förfrågan till https://example.com/api/data
    .then(response => response.json()) //Konvertera svaret till JSON format
    .then(data => console.log(data)) //Logga ut svaret till konsolen
```

Output:
```TypeScript 
{ 
    name: "John", 
    age: 25, 
    city: "Stockholm" 
}
```

## Djupdykning:
HTTP (HyperText Transfer Protocol) är en protokoll som används för att överföra data mellan en webbsida och en server. Det används vanligtvis för att hämta HTML-sidor och andra resurser från en webbplats. En alternativ metod för att skicka HTTP förfrågningar är att använda ett tredjepartsbibliotek, som axios eller request. När en HTTP förfrågan görs, skickas det som ett HTTP-verb, som GET, POST eller PUT, för att indikera syftet med förfrågan. 

## Se Även: 
https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch \
https://www.twilio.com/blog/2017/08/http-requests-in-typescript.html \
https://www.npmjs.com/package/axios