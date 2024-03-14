---
date: 2024-01-20 18:02:07.876945-07:00
description: null
lastmod: '2024-03-13T22:44:41.183441-06:00'
model: gpt-4-1106-preview
summary: null
title: "\xC5 sende en HTTP-foresp\xF8rsel med grunnleggende autentisering"
---

{{< edit_this_page >}}

# Hva og hvorfor?

Å sende en HTTP-forespørsel med grunnleggende autentisering betyr at du inkluderer brukernavn og passord i forespørselshodet for å få tilgang til en ressurs. Programmerere bruker dette for sikker tilgang og for å identifisere brukere.

# Slik gjør du:

```Javascript
const axios = require('axios');
const base64 = require('base-64');

let username = 'dinbruker';
let password = 'dittPassord';
let basicAuth = 'Basic ' + base64.encode(username + ':' + password);

axios.get('http://eksempel.no/data', { headers: { 'Authorization': basicAuth } })
  .then(response => {
    console.log(response.data);
  })
  .catch(error => {
    console.error('Autentisering feilet: ', error);
  });
```

Eksempelutdata:

```
{ "id": 1, "navn": "Ola Nordmann", "data": "Hemmelig informasjon" }
```

# Dypdykk

I gamle dager var grunnleggende autentisering over HTTP standard for å beskytte tilgangen. Men det er ikke så sikkert siden det er lett å avkode Base64-koding. I dag er alternativer som OAuth 2.0 og JWT (JSON Web Tokens) ofte brukt fordi de tilbyr større sikkerhet. Med grunnleggende autentisering, må du alltid bruke HTTPS for å kryptere brukerdata over nettverket.

Grunnleggende autentisering er enkel å implementere og forstå. Det krever bare at serveren sjekker 'Authorization'-hodet i HTTP-forespørselen. Men pass på: Lagre aldri passord i klar tekst i din kode. Bruk miljøvariabler eller en hemmelighetsbehandlingstjeneste for å holde din autentiseringsinfo sikker.

# Se også:

- MDN Web Docs om grunnleggende autentisering: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#basic_authentication_scheme
- Axios GitHub repo for HTTP-forespørsler: https://github.com/axios/axios
- Sikkerhetspraksis for autentisering: https://owasp.org/www-project-top-ten/
