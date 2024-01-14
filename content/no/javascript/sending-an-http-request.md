---
title:                "Javascript: Sende en http-forespørsel"
simple_title:         "Sende en http-forespørsel"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Hvorfor

Å sende en HTTP-request er en viktig del av webutvikling. Ved å bruke Javascript for å sende disse forespørslene, kan du lage dynamiske og responsive nettsider som kommuniserer med ulike servere og databaser. Dette gjør det mulig å hente og sende informasjon i sanntid, noe som er avgjørende for online interaktivitet.

## Hvordan

For å sende en HTTP-request i Javascript, kan du bruke den innebygde `XMLHttpRequest`-objektet. Først må du opprette et nytt objekt ved å kalle på `XMLHttpRequest()`-funksjonen. Deretter kan du konfigurere forespørselen ved å angi en metode (f.eks. GET eller POST) og en URL.

```javascript
let request = new XMLHttpRequest(); // Oppretter et nytt objekt
request.open("GET", "https://api.example.com/users"); // Konfigurerer forespørselen
```

Neste steg er å sende forespørselen ved å kalle `send()`-metoden på objektet. Hvis forespørselen er vellykket, vil du få en respons fra serveren som inneholder informasjonen du har bedt om.

```javascript
request.send(); // Sender forespørselen
console.log(request.responseText); // Skriver ut responsen fra serveren
```

Outputen vil avhenge av hvilken informasjon du har bedt om fra serveren. I dette tilfellet vil `console.log` vise dataene fra brukerlisten på API-et.

## Dypdykk

Hvis du vil sende mer komplekse HTTP-forespørsler, kan du også bruke biblioteker som Axios eller jQuery. Disse gir deg flere metoder og funksjoner for å konfigurere og håndtere forespørsler, og gjør det ofte enklere å håndtere ulike dataformater som JSON.

Det er også viktig å være klar over sikkerhetsrisikoer når du sender en HTTP-request, spesielt hvis det inneholder sensitiv informasjon. Det er derfor viktig å alltid bruke HTTPS hvis mulig og å beskytte din kode mot angrep som Cross-Site Request Forgery (CSRF).

## Se også

- [Axios](https://github.com/axios/axios)
- [jQuery.ajax()](https://api.jquery.com/jquery.ajax/)
- [Sikkerhetsrisikoer ved HTTP-requests](https://portswigger.net/web-security/csrf)
- [Guide til HTTP og nettverksforespørsler](https://developer.mozilla.org/nb/docs/Web/HTTP)