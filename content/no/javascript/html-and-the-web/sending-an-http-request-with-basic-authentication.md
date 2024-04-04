---
changelog:
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-20 18:01:57.938512-07:00
description: 'Hvordan: Her er et raskt eksempel med bruk av JavaScripts Fetch API.'
lastmod: '2024-04-04T00:26:49.361956-06:00'
model: gpt-4-0125-preview
summary: Her er et raskt eksempel med bruk av JavaScripts Fetch API.
title: "Sende en HTTP-foresp\xF8rsel med grunnleggende autentisering"
weight: 45
---

## Hvordan:
Her er et raskt eksempel med bruk av JavaScripts Fetch API:

```javascript
const url = 'https://some-protected-resource.com/data';
const brukernavn = 'DittBrukernavn';
const passord = 'DittPassord';

const headers = new Headers();
headers.set('Authorization', 'Basic ' + btoa(brukernavn + ':' + passord));

fetch(url, { method: 'GET', headers: headers })
  .then(response => {
    if (response.ok) return response.json();
    throw new Error('Nettverkssvaret var ikke ok.');
  })
  .then(data => console.log(data))
  .catch(error => console.error('Fetch-feil: ', error));
```

Eksempel på utskrift (skrevet ut til konsollen):

```javascript
{
  "beskyttet": "data",
  "merData": 12345
}
```

## Dypdykk
Før vi dykker ned, la oss få litt kontekst. Grunnleggende autentisering er en av de enkleste formene for websikkerhet og sender legitimasjon i headere med hver forespørsel.

Historisk kontekst:
- Grunnleggende HTTP-autentisering er en gammel metode, opprinnelig skissert i RFC 7617 fra 2015, som erstatter den enda eldre RFC 2617 fra 1999.
- Den var mye brukt på grunn av sin enkelhet, men er ikke like sikker uten HTTPS, ettersom base64-koding lett kan reverseres.

Alternativer:
- OAuth: En sikrere og mer kompleks standard for tilgangsdelegering, brukt i tilfeller hvor du trenger å gi tilgang uten å dele passordlegitimasjon.
- API-nøkler: Et enkelt token som er lettere å håndtere enn komplekse OAuth-protokoller.
- Bærertokens: Spesielt JWT (JSON Web Tokens), som kan bære mer informasjon.

Implementeringsdetaljer:
- Base64-koding omdanner strengen brukernavn:passord til en sekvens av tegn som er mer universelt overførbar.
- Forsikre deg alltid om at tilkoblingen er HTTPS, for å forhindre at legitimasjon blir avlyttet.
- Moderne utvikling foretrekker tokens og sesjonscookies for autentisering, da de er sikrere og mer allsidige.

## Se også
- [Mozilla Developer Network - Autorisasjon](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Authorization)
- [RFC 7617 - HTTP Basic Auth](https://tools.ietf.org/html/rfc7617)
- [Introduksjon til OAuth 2.0](https://www.digitalocean.com/community/tutorials/an-introduction-to-oauth-2)
- [JSON Web Tokens (JWT)](https://jwt.io/introduction/)
