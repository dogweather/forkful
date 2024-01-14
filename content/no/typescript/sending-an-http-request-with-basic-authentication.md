---
title:                "TypeScript: Å sende en http-forespørsel med grunnleggende autentisering"
simple_title:         "Å sende en http-forespørsel med grunnleggende autentisering"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Hvorfor

Å sende HTTP-forespørsler med grunnleggende autentisering er en vanlig praksis innen webutvikling for å sikre at bare autoriserte brukere kan få tilgang til sensitive data eller handlinger. Dette er spesielt viktig for applikasjoner som håndterer brukerdata eller som krever brukerens innlogging for å utføre visse oppgaver.

# Hvordan

For å sende en HTTP-forespørsel med grunnleggende autentisering i TypeScript, kan du bruke biblioteket axios. Dette kan installeres ved å kjøre kommandoen ```npm install axios``` i terminalen. Deretter kan du importere axios-biblioteket i koden din ved å skrive ```import axios from 'axios'```.

Når du har importert biblioteket, kan du bruke følgende kode for å sende en GET-forespørsel med grunnleggende autentisering til en bestemt URL:

```TypeScript
axios.get('https://eksempelurl.com/api/data', {
    auth: {
        username: 'brukernavn',
        password: 'passord'
    }
})
.then(response => {
    console.log(response.data);
})
.catch(error => {
    console.log(error);
})
```

Her bruker vi auth-metoden fra axios for å sende brukernavn og passord som en del av forespørselen. Dette genererer en base64-kryptert streng som sendes sammen med forespørselen for å autentisere brukeren.

I dette eksemplet blir responsen fra serveren logget til konsollen. Du kan også bruke andre HTTP-metoder som POST, PUT eller DELETE på samme måte.

# Dykk dypere

Når du sender en HTTP-forespørsel med grunnleggende autentisering, må du sørge for å kryptere både brukernavnet og passordet. Dette kan gjøres ved hjelp av base64-kryptering, som konverterer en streng til en ASCII-streng. Men selv om denne krypteringsmetoden gjør det vanskeligere for uautoriserte å lese innholdet, er det ikke en helt sikker metode for autentisering. Det er derfor viktig å også implementere andre sikkerhetstiltak, som for eksempel HTTPS-protokollen, for å sikre at dataene dine er trygge.

# Se også

- [Axios-dokumentasjon](https://github.com/axios/axios)
- [Base64-kryptering](https://developer.mozilla.org/en-US/docs/Web/API/WindowOrWorkerGlobalScope/btoa)
- [Sikkerhetstiltak for HTTP-forespørsler](https://developer.mozilla.org/en-US/docs/Web/HTTP/Security)