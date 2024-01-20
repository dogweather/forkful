---
title:                "Å sende en http-forespørsel"
html_title:           "C++: Å sende en http-forespørsel"
simple_title:         "Å sende en http-forespørsel"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

En HTTP-forespørsel er en melding sendt fra en klient (din nettleser) til en server. Programmerere sender HTTP-forespørsler for å hente eller sende data til en server.

## Hvordan gjøre det:

Her er ledelsen. For å sende en HTTP GET-forespørsel i JavaScript, bruker vi `fetch()`-metoden. Response kan leses og behandlet som JSON, som vises i efterfølgende eksempel:

```Javascript
fetch("https://api.example.com/data")
    .then(response => response.json())
    .then(data => console.log(data))
    .catch(error => console.log("Error:", error));
```
I koden over sender vi en GET-forespørsel til "api.example.com/data". Responsen konverteres til JSON og deretter logget til konsollen. Hvis det oppstår en feil, vil den også bli logget til konsollen.

## Dypdykk:

- **Historisk kontekst:** I tidlige dager av webben, var AJAX (Asynchronous JavaScript and XML) gullstandarden for å sende HTTP-forespørsler. Med introduksjonen av Fetch API, er det nå enklere og mindre rotete å håndtere asynkrone forespørsler med moderne JavaScript.
- **Alternativer:** Selv om `fetch()` er innebygd og enkel å bruke, finnes det alternativer som Axios, jQuery ($.ajax), og den eldre `XMLHttpRequest`. Disse bibliotekene kan ha flere funksjoner og kan være lettere å bruke i visse situasjoner.
- **Gjennomføringsdetaljer:** Detaljene om hvordan HTTP-forespørsler blir sendt og håndtert avhenger av mange faktorer, inkludert nettleseren, serverinnstillingene, og eventuelle mellomvare.

## Se også:

- [MDN Web Docs: bruk av Fetch](https://developer.mozilla.org/no/docs/Web/API/Fetch_API/Using_Fetch)
- [JavaScript.info: Fetch](https://javascript.info/fetch)
- [Axios på GitHub](https://github.com/axios/axios)