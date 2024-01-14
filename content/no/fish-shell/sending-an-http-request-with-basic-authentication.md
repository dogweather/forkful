---
title:                "Fish Shell: Sending en http forespørsel med grunnleggende autentisering"
simple_title:         "Sending en http forespørsel med grunnleggende autentisering"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hvorfor
HTTP-forespørsler med grunnleggende godkjenning er viktig for å sikre sikkerheten til nettjenester. Dette kan være nyttig for å begrense tilgangen til sensitive data eller funksjoner, og å sikre at bare autoriserte brukere får tilgang til tjenesten.

## Hvordan gjøre det
For å sende en HTTP-forespørsel med grunnleggende godkjenning i Fish Shell, kan du bruke følgende kodeblokk:

```Fish Shell
curl -u <brukernavn>:<passord> <URL>
```

Dette vil resultere i en forespørsel til den angitte URL-en med brukernavn og passord inkludert i HTTP-overskriften.

For å utføre en GET-forespørsel, kan du bruke følgende kode:

```Fish Shell
curl -u <brukernavn>:<passord> -X GET <URL>
```

Dette vil sende en GET-forespørsel med brukernavn og passord i overskriften. Du kan også bruke andre metoder som POST, PUT, DELETE og mer ved å endre verdien for -X-flagget.

## Dypdykk
For å forstå det grundigere, la oss ta en titt på hvordan det fungerer. HTTP-forespørsler med grunnleggende godkjenning bruker en Base64-kodet brukernavn og passord som en del av autorisasjonsheaderen i HTTP-forespørselen. Når serveren mottar forespørselen, dekodes denne informasjonen for å bekrefte identiteten til brukeren.

Det er viktig å merke seg at Base64-koding ikke er en sikker form for kryptering, men bare en måte å konvertere tekst til binærkode. Det er derfor ikke anbefalt å bruke dette for sensitiv informasjon som passord.

## Se også
- [Fish Shell dokumentasjon](https://fishshell.com/docs/current/cmds/curl.html)
- [Hvordan bruke Curl for å sende HTTP-forespørsler](https://www.digitalocean.com/community/tutorials/how-to-use-curl-to-send-and-receive-http-requests)