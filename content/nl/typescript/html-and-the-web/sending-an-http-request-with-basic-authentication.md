---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:15.627684-07:00
description: "Hoe te: Er was eens, voordat OAuth en JWTs de scene overnamen, was basisauth\
  \ de go-to. Het is nog steeds handig voor interne gereedschappen of Proof of\u2026"
lastmod: '2024-04-05T22:51:03.388496-06:00'
model: gpt-4-0125-preview
summary: Er was eens, voordat OAuth en JWTs de scene overnamen, was basisauth de go-to.
title: Een HTTP-verzoek verzenden met basisauthenticatie
weight: 45
---

## Hoe te:
```typescript
import axios from 'axios';

// codeer je gebruikersnaam en wachtwoord
const token = Buffer.from('jeGebruikersnaam:jeWachtwoord').toString('base64');
const url = 'https://jouw.api/eindpunt';

// stel het HTTP-verzoek in met Axios
axios.get(url, {
  headers: {
    'Authorization': `Basic ${token}`
  }
})
.then(response => {
  console.log(response.data); // dit is je verwachte uitvoer
})
.catch(error => {
  console.error("Oeps, er ging iets mis!", error);
});
```

Voorbeelduitvoer:

```
{ "message": "Je bent binnen! Welkom in het geheime API-land." }
```

## Diepgaand
Er was eens, voordat OAuth en JWTs de scene overnamen, was basisauth de go-to. Het is nog steeds handig voor interne gereedschappen of Proof of Concepts (PoCs). Het idee is eenvoudig: voeg een kop toe met 'Authorization', gebruik 'Basic ' + een base64 gecodeerde 'gebruikersnaam:wachtwoord'. Voilà, je bent door de poorten.

Maar het is niet allemaal rozengeur en maneschijn. Er zijn risico's - als je geen HTTPS gebruikt, schreeuw je praktisch je inloggegevens hardop uit. Alternatieven? OAuth2-tokens, JWTs, API-sleutels - ze zijn als sterkere, stille types. Ze dienen vergelijkbare doeleinden maar met meer complexiteit en veiligheid.

Bij het implementeren van basisauth in TypeScript, is de gangbare keuze `axios` of `fetch`. In ons geval maakt `axios` het instellen van aangepaste koppen een fluitje van een cent. Bovendien retourneert het promises, waardoor het een droom is met `async/await`.

Houd in gedachten: 'Basic' zal zijn leeftijd snel onthullen in het moderne web waar HTTPS een must is en veiligheidsnormen hoger zijn. Toch, voor interne netwerken of waar hogere beveiliging niet cruciaal is, is het een makkie.

## Zie Ook
Voor meer authenticatiemethoden en beveiligingsbest practices:

- [MDN Web Docs: Autorisatie](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Authorization)
- [OWASP Authenticatie Cheat Sheet](https://cheatsheetseries.owasp.org/cheatsheets/Authentication_Cheat_Sheet.html)
- Officiële `axios` documentatie voor aangepaste HTTP-headers: [Axios Docs](https://axios-http.com/docs/req_config)
