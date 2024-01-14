---
title:                "Fish Shell: Å sende en http-forespørsel"
simple_title:         "Å sende en http-forespørsel"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/sending-an-http-request.md"
---

{{< edit_this_page >}}

##Hvorfor

I dagens digitale verden er det en vanlig utfordring å kommunisere med eksterne tjenester og API-er. Dette kan være alt fra å hente data fra et nettsted til å integrere tjenester i en app. For å gjøre dette trenger man ofte å sende en forespørsel over HTTP-protokollen. Med Fish Shell kan du enkelt sende HTTP-forespørsler direkte fra terminalen uten å måtte åpne en nettleser eller installere ekstra programmer. Det er også et godt verktøy for å teste eksisterende API-er.

##Slik gjør du det

For å sende en HTTP-forespørsel med Fish Shell, kan du bruke kommandoen `curl` etterfulgt av nettadressen du ønsker å sende forespørselen til. For eksempel:

```Fish Shell
curl https://www.example.com/
```

Dette vil sende en GET-forespørsel og du vil få tilbake HTML-koden fra nettsiden som respons. Du kan også legge til flere parametere som for eksempel brukernavn og passord hvis du må autentisere deg for å få tilgang til tjenesten.

```Fish Shell
curl -u brukernavn:passord https://www.example.com/
```

Ønsker du å sende en POST-forespørsel kan du bruke flagget `-X` etterfulgt av `POST` og legge til eventuelle data som skal sendes med forespørselen.

```Fish Shell
curl -X POST -d 'navn=John&alder=30' https://www.example.com/api
```

Du kan også sende forespørsler med andre metoder som PUT, DELETE og HEAD ved å bruke samme syntaks og bare endre `POST` til den ønskede metoden.

##Dypdykk

Det finnes også andre måter å sende HTTP-forespørsler på med Fish Shell, som for eksempel ved hjelp av kommandoen `wget` eller ved å bruke Fish Shell-modulen `httpie`. Det er også mulig å sette tilpassede HTTP-headerfelt eller utføre flertrådede forespørsler.

Hvis du ønsker å utforske mer avanserte måter å håndtere HTTP-forespørsler på, kan du sjekke ut dokumentasjonen til Fish Shell eller søke etter ekstra moduler som kan gjøre denne prosessen enklere og mer fleksibel.

##Se også

- Offisiell Fish Shell dokumentasjon for HTTP-forespørsler (https://fishshell.com/docs/current/cmds/curl.html)
- Fish Shell-modulen httpie (https://github.com/xxh3x/httpie-fish)
- En guide for å bruke HTTP-forespørsler med Fish Shell (https://www.linux.com/topic/networking/using-http-requests-fish-shell/)