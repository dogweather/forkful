---
title:                "Å laste ned en nettside"
html_title:           "Javascript: Å laste ned en nettside"
simple_title:         "Å laste ned en nettside"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hvorfor

Å laste ned en nettside kan være nødvendig for å lagre informasjon eller bearbeide dataene videre. Dette kan være nyttig for forskning, nettsideanalyse, eller å bare ha tilgang til nettsiden offline.

## Slik gjør du det

For å laste ned en nettside i Javascript, kan du bruke XMLHttpRequest-objektet. Her er et enkelt eksempel:

```javascript
var xhr = new XMLHttpRequest();
xhr.open('GET', 'https://www.example.com');
xhr.send();

xhr.onload = function () {
  if (xhr.status == 200) { // Sjekker om forespørselen var vellykket
    console.log(xhr.responseText); // Skriver ut nettsiden
  }
};
```

I dette eksempelet opprettes en ny HTTP-forespørsel ved hjelp av GET-metoden, som henter nettsiden fra URL-en som er angitt som parameter. Deretter sendes forespørselen og ved hjelp av onload funksjonen kan du få tilgang til nettsidens innhold gjennom responseText egenskapen.

## Dypdykk

Hvis du ønsker å få tilgang til mer databehandlingskraft, kan du bruke Axios biblioteket. Dette gjøre det enklere å håndtere ulike nettverksforespørsler og kan gjøre det lettere å håndtere feil og sikkerhetstiltak.

Et annet alternativ er å bruke Node.js, som lar deg manipulere nettsidekoden og hente ut spesifikke elementer ved hjelp av biblioteker som Cheerio og Puppeteer.

## Se også

- [XMLHttpRequest](https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest)
- [Axios](https://github.com/axios/axios)
- [Node.js](https://nodejs.org/en/)
- [Cheerio](https://cheerio.js.org/)
- [Puppeteer](https://pptr.dev/)