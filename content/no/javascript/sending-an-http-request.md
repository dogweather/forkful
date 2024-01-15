---
title:                "Å sende en http-forespørsel"
html_title:           "Javascript: Å sende en http-forespørsel"
simple_title:         "Å sende en http-forespørsel"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du vil hente data fra en webside, eller sende data til en annen server, må du gjøre en HTTP-request. Dette er en grunnleggende handling i moderne webutvikling.

## Hvordan

For å sende en HTTP-request i Javascript, kan vi bruke et innebygd objekt kalt XMLHttpRequest. Her er et eksempel som sender en GET-request til API-et til Github:

``` Javascript
var xhr = new XMLHttpRequest(); // Opprett en ny XMLHttpRequest-instans
xhr.open('GET', 'https://api.github.com/users/username'); // Angi URL-en og HTTP-metoden
xhr.send(); // Send requesten

// Venter på respons fra serveren
xhr.onload = function() {
  if (xhr.status == 200) { // Sjekker at requesten var vellykket
    console.log(xhr.response); // Logger responsen (i dette tilfellet data om brukeren med brukernavnet 'username')
  }
};
```

Output:

``` 
{
    "login": "username",
    "id": 123456,
    "avatar_url": "https://avatars.githubusercontent.com/u/123456?v=4",
    "name": "John Smith",
    "company": null,
    "blog": "https://example.com",
    "location": "Norway",
    "email": null,
    ...
}
```

Dette er et enkelt eksempel på hvordan man kan sende en HTTP-request ved hjelp av JavaScript. Det finnes også andre måter å gjøre dette på, for eksempel ved bruk av jQuery eller fetch API-et.

## Dypdykk

Det finnes ulike typer HTTP-requests, som GET, POST, PUT, DELETE, etc. Disse representerer ulike handlinger man kan utføre mot en server. Det finnes også forskjellige HTTP-statuskoder som kan returneres av en server, som 200 (OK), 404 (Not Found), 500 (Internal Server Error), etc. Det er viktig å forstå disse konseptene når man jobber med HTTP-requests i JavaScript for å sikre at requestene våre er pålitelige og håndterer eventuelle feil som kan oppstå.

## Se også

- MDN webdocs: [Making HTTP requests using XMLHttpRequest](https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest/Using_XMLHttpRequest)
- W3Schools: [XMLHttpRequest Object](https://www.w3schools.com/js/js_ajax_http.asp)
- jQuery documentation: [jQuery.get()](https://api.jquery.com/jquery.get/)
- Fetch API: [Introduction to fetch()](https://developers.google.com/web/updates/2015/03/introduction-to-fetch)