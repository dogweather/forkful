---
date: 2024-01-20 17:45:04.567289-07:00
description: "Miten: Voit k\xE4ytt\xE4\xE4 Node.js:n `axios`-kirjastoa web-sivujen\
  \ lataamiseen TypeScriptiss\xE4. T\xE4ss\xE4 yksinkertainen esimerkki."
lastmod: '2024-03-13T22:44:56.314207-06:00'
model: gpt-4-1106-preview
summary: "Voit k\xE4ytt\xE4\xE4 Node.js:n `axios`-kirjastoa web-sivujen lataamiseen\
  \ TypeScriptiss\xE4."
title: Verkkosivun lataaminen
weight: 42
---

## Miten:
Voit käyttää Node.js:n `axios`-kirjastoa web-sivujen lataamiseen TypeScriptissä. Tässä yksinkertainen esimerkki:

```TypeScript
import axios from 'axios';

async function downloadPage(url: string): Promise<string> {
  try {
    const response = await axios.get(url);
    return response.data;
  } catch (error) {
    console.error('Virhe latauksessa:', error);
    return '';
  }
}

const url = 'https://esimerkki.fi';
downloadPage(url).then(data => {
  console.log(data);
});
```

Kääntämisen jälkeen ja suoritettaessa, konsoliin tulostuu ladatun web-sivun HTML-koodi.

## Syväsukellus:
Web-sivujen lataaminen ohjelmallisesti on ollut mahdollista jo vuosien ajan, alkaen yksinkertaisista HTTP-kirjastoista kuten `curl` ja `wget` aina nykyaikaisiin HTTP-asiakaskirjastoihin. `axios` on suosittu valinta TypeScriptin ja JavaScriptin keskuudessa helppokäyttöisyytensä ja lupapohjaisen (promise-based) syntaksinsa johdosta. Vaihtoehtoisesti voisi käyttää sisäänrakennettua `http`- tai `https`-moduulia Node.js:ssä, mutta ne vaativat yleensä enemmän konfiguraatiota ja koodia. On tärkeää huomioida tekijänoikeudet ja palveluiden käyttöehdot sivuja ladataessa.

## Katso Myös:
- Axios GitHub-sivu: https://github.com/axios/axios
- MDN Web Docs - HTTP-asiakkaan luoaminen: https://developer.mozilla.org/en-US/docs/Web/HTTP/Clients
- Node.js `http`-moduuli: https://nodejs.org/api/http.html
