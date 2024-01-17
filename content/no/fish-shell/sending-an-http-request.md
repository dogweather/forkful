---
title:                "Sending en http-forespørsel"
html_title:           "Fish Shell: Sending en http-forespørsel"
simple_title:         "Sending en http-forespørsel"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Når vi snakker om å sende en HTTP-forespørsel, mener vi at vi ber en server om å gjøre en bestemt handling for oss. Dette kan være alt fra å hente data fra en nettside til å legge til en ny bruker i en database. Programmere bruker dette for å effektivt kommunisere med servere og automatisere oppgaver som ellers ville vært tidkrevende å gjøre manuelt.

## Slik gjør du det:

Sending av en HTTP-forespørsel i Fish Shell er veldig enkelt. Du trenger bare å bruke `curl` kommandoen, etterfulgt av nettadressen du ønsker å sende forespørselen til. Her er et eksempel som henter informasjon fra nettsiden "http://www.example.com":

```
curl http://www.example.com
```

Dette vil returnere nettsidens HTML-kode i terminalen.

## Dypdykk:

HTTP-protokollen ble opprettet på 90-tallet som en standard for å kommunisere mellom klienter og servere på internett. Selv om `curl` er et vanlig valg for å sende HTTP-forespørsler i Fish Shell, finnes det også alternativer som `wget` og `httpie`. Det er også verdt å merke seg at Fish Shell støtter å bruke kommandoen `fetch` for å sende HTTP-forespørsler, men denne er ikke like utbredt som `curl`.

## Se også:

For mer informasjon om `curl` og HTTP-forespørsler, sjekk ut disse kildene:

- [Offisiell informasjon om `curl`](https://curl.haxx.se/docs/manpage.html)
- [HTTP-forespørsler på Wikipedia](https://en.wikipedia.org/wiki/Hypertext_Transfer_Protocol#Request_methods)
- [Fish Shell dokumentasjon om `fetch`](https://fishshell.com/docs/current/commands.html#fetch)