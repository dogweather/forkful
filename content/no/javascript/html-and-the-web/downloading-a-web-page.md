---
title:                "Nedlasting av en nettside"
aliases: - /no/javascript/downloading-a-web-page.md
date:                  2024-01-20T17:44:13.695964-07:00
model:                 gpt-4-1106-preview
simple_title:         "Nedlasting av en nettside"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?
Nettsider lastes ned for å hente og bruke data eller innhold uten en nettleser. Programmerere gjør dette for å analysere informasjonen, teste nettsider eller integrere data i egne applikasjoner.

## How to:
Node.js er verktøyet vi bruker. Med `axios`, kan vi laste ned en nettside enkelt. Her er et eksempel:

```javascript
const axios = require('axios');

async function downloadPage(url) {
  try {
    const response = await axios.get(url);
    console.log(response.data); // Skriver ut HTML-innholdet til nettsiden
  } catch (error) {
    console.error('Oops, noe gikk galt!', error);
  }
}

downloadPage('https://www.example.com');
```

Sample output for example.com:

```javascript
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</html>
```

## Deep Dive
I gamle dager brukte vi `XMLHttpRequest` men Node.js introduserte `http` og `https` moduler. Disse fungerer, men bibliotek som `axios` eller `fetch` (med `node-fetch`) er mer moderne og håndterer JSON-data bedre. 

Et alternativ til `axios` er `puppeteer` for når du trenger å simulere en nettleser og håndtere JavaScript-rendering på siden, noe `axios` ikke kan.

Implementasjonen din kan variere avhengig av behovet for feilhåndtering, støtte for informasjonskapsler, HTTP-headers, etc. `axios` er populært fordi det gir et løftebasert API og håndterer kryssplattform-forespørsler godt.

## See Also
- [Axios GitHub repository](https://github.com/axios/axios)
- [Node.js http documentation](https://nodejs.org/api/http.html)
- [Puppeteer GitHub repository](https://github.com/puppeteer/puppeteer)
- [npm node-fetch package](https://www.npmjs.com/package/node-fetch)
