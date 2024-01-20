---
title:                "Laste ned en nettside"
html_title:           "Elixir: Laste ned en nettside"
simple_title:         "Laste ned en nettside"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å laste ned en nettside er prosessen med å hente all data fra en spesifikk URL, som ofte inkluderer HTML, CSS, JavaScript, bilder, osv. Programmerere gjør dette for å analysere og jobbe med nettsidens data offline.

## Hvordan gjøre det:

I TypeScript kan du bruke Node.js-biblioteket `axios` for å laste ned en nettside. Her er en enkel kodeblokk:

```TypeScript
import axios from 'axios';

async function last_ned_nettside(url: string): Promise<string> {
    const respons = await axios.get(url);
    return respons.data;
}

// Eksempel bruk:
last_ned_nettside("https://google.com")
    .then(data => console.log(data))
    .catch(err => console.error(err));
```

Når du kjører denne koden, vil du se innholdet i Google-hjemmesiden skrive ut i konsollen.

## Dypdykk:

**Historisk kontekst:** Nedlasting av websider har vært en viktig del av programmering siden internettets spede begynnelse. Det tillater datamaskiner å hente og bearbeide informasjon fra nettet.

**Alternativer:** `axios` er et populært bibliotek, men det er mange andre metoder for å laste ned en nettside i TypeScript, som `node-fetch` og den innebyggede `http`-modulen i Node.js.

**Implementeringsdetaljer:** Når du laster ned en nettside, gjør du faktisk en HTTP GET forespørsel til en server. Serveren returnerer deretter en respons, som inneholder nettsidens data.

## Se også:

- Axios dokumentasjon: [https://axios-http.com/docs/intro](https://axios-http.com/docs/intro)
- Node.js HTTP Modul: [https://nodejs.org/api/http.html](https://nodejs.org/api/http.html)
- Tutorial om hvordan gjøre HTTP forespørsler i Node.js: [https://www.twilio.com/blog/2017/08/http-requests-in-node-js.html](https://www.twilio.com/blog/2017/08/http-requests-in-node-js.html)