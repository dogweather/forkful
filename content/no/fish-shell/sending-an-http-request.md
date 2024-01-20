---
title:                "Å sende en http-forespørsel"
html_title:           "C++: Å sende en http-forespørsel"
simple_title:         "Å sende en http-forespørsel"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å sende en HTTP-forespørsel betyr å be en server, vanligvis en webserver, om data eller å utføre en bestemt oppgave. Programmerere gjør dette for å hente, manipulere eller arkivere data på nett.

## Hvordan gjøre dette:
Her er et eksempel på hvordan du sender en GET-forespørsel med curl i Fish Shell:

```Fish Shell
curl http://httpbin.org/get
```
Svar: 
```Fish Shell
{
  "args": {}, 
  "headers": {
    "Accept": "*/*", 
    "Host": "httpbin.org", 
    "User-Agent": "curl/7.64.1",
    "X-Amzn-Trace-Id": "Root=1-61037102-5d225b9bb8bbe648e9b3d5f4"
  }, 
  "origin": "79.160.243.91, 79.160.243.91", 
  "url": "http://httpbin.org/get"
}
```
## Dypdykk:
Historisk sett har HTTP forespørsler vært sentrale i mange former for nettbasert kommunikasjon og er en hjørnestein i moderne webutvikling. Utover 'curl', finnes det flere alternative verktøy for å sende HTTP-forespørsler, som 'wget' og 'HTTPie'. Ved å sende HTTP-forespørsler via Fish Shell, kan du enkelt integrere nettbaserte tjenester i skriptene dine. For eksempel kan du automatisk hente data fra en REST API, eller oppdatere statusen på en online tjeneste.

## Se også:
- [HTTP-forespørsler med Fish Shell - Offisiell dokumentasjon](https://fishshell.com/docs/current/commands.html)
- [HTTPie](https://httpie.io/)