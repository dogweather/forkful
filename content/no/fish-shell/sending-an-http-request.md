---
title:                "Å sende en http-forespørsel"
html_title:           "Fish Shell: Å sende en http-forespørsel"
simple_title:         "Å sende en http-forespørsel"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor ville noen ønske å sende en HTTP forespørsel? Vel, det er mange grunner! Det kan være for å hente data fra en ekstern API, sende data til en annen server, eller til og med lage ditt eget HTTP baserte program. Uansett hva din grunn måtte være, er det viktig å ha en pålitelig og enkel måte å sende HTTP forespørsler på. Det er her Fish Shell kommer inn i bildet.

## Hvordan gjøre det

Å sende en HTTP forespørsel er faktisk ganske enkelt med Fish Shell. Alt du trenger å gjøre er å bruke kommandoen `curl` etterfulgt av nettadressen du ønsker å sende forespørselen til. La oss for eksempel si at vi ønsker å få dataene fra Github sitt API. Da kan vi skrive følgende i terminalen:

```Fish Shell
curl https://api.github.com/users/github
```

Dette vil sende en GET forespørsel til Github sitt API og returnere informasjon om brukeren "github". Hvis du ønsker å sende med noen data, for eksempel som del av en POST forespørsel, kan du bruke flagget `-d` etterfulgt av dataene du vil sende.

```Fish Shell
curl -d "navn=John&alder=30" http://www.example.com
```

Dette vil sende en POST forespørsel til http://www.example.com med dataene "navn=John&alder=30" i kroppen.

Det er også mulig å legge til ekstra header informasjon ved å bruke flagget `-H` og spesifisere hvilken header du ønsker å legge til.

```Fish Shell
curl -H "Content-Type: application/json" http://www.example.com
```

Dette vil legge til en "Content-Type" header med verdien "application/json" i forespørselen.

## Dypdykk

For de som er mer erfarne med HTTP forespørsler, vil du være glad for å vite at Fish Shell støtter mange av de vanlige metodeene, som GET, POST, PUT, PATCH og DELETE. Du kan også angi egendefinerte headers og data, som nevnt tidligere.

En annen nyttig funksjon i Fish Shell er evnen til å utføre scripting mens du sender en HTTP forespørsel. Dette betyr at du kan bruke variabler eller skrive logikk inne i kommandoen `curl`.

```Fish Shell
set token (echo $GIT_TOKEN) # Henter et access token fra miljøvariabler
curl -H "Authorization: token $token" https://api.github.com/user/repos # Legger til token som en Authorization header
```

Dette er bare noen få eksempler på hvordan du kan bruke Fish Shell for å sende HTTP forespørsler. Det er mange flere funksjoner og muligheter, så det er bare å eksperimentere og finne ut hva som fungerer best for deg!

## Se også

- [Fish Shell dokumentasjon](https://fishshell.com/docs/current/)
- [Curl dokumentasjon](https://curl.se/docs/manpage.html)