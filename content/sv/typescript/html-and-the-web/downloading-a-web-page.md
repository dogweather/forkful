---
date: 2024-01-20 17:45:00.865969-07:00
description: "Hur man g\xF6r: Att h\xE4mta webbsidor \xE4r en vanlig uppgift som kan\
  \ utf\xF6ras med olika verktyg och bibliotek. Historiskt har verktyg som `curl`\
  \ och `wget`\u2026"
lastmod: '2024-04-05T22:50:51.939988-06:00'
model: gpt-4-1106-preview
summary: "Att h\xE4mta webbsidor \xE4r en vanlig uppgift som kan utf\xF6ras med olika\
  \ verktyg och bibliotek."
title: "H\xE4mta en webbsida"
weight: 42
---

## Hur man gör:
```TypeScript
import axios from 'axios';  // För HTTP-begäranden

async function laddaNerWebbsida(url: string): Promise<string> {
    try {
        const response = await axios.get(url);
        return response.data;  // Webbsidans HTML som en sträng
    } catch (error) {
        throw new Error(`Kunde inte ladda ner sidan: ${error}`);
    }
}

// Använd funktionen och skriv ut resultatet
(async () => {
    const webbsida = await laddaNerWebbsida('https://www.exempel.se');
    console.log(webbsida);
})();
```
Exempelutdata:
```
<!doctype html>
<html lang="sv">
...
</html>
```

## Djupdykning:
Att hämta webbsidor är en vanlig uppgift som kan utföras med olika verktyg och bibliotek. Historiskt har verktyg som `curl` och `wget` använts i kommandotolkar. I TypeScript och JavaScript-världen är `axios`, en populär HTTP-klient, och `fetch`-API:et i moderna webbläsare, vanliga val. 

Detaljerna i implementationen kan variera beroende på sidans struktur och skyddsmekanismer, som CORS (Cross-Origin Resource Sharing) eller CSRF-token (Cross-Site Request Forgery). För att hantera dynamiska webbsidor som används JavaScript kan ett huvudlöst webbläsarbibliotek som Puppeteer behövas.

## Se även:
- Axios dokumentation: https://axios-http.com/docs/intro
- MDN web docs om `fetch`: https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch
- Puppeteer GitHub-sida: https://github.com/puppeteer/puppeteer
