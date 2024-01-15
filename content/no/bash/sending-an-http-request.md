---
title:                "Sende en http-forespørsel"
html_title:           "Bash: Sende en http-forespørsel"
simple_title:         "Sende en http-forespørsel"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Hvorfor

Sending av HTTP-forespørsler er en viktig del av å lage automatiserte prosesser som eksisterer bak kulissene på nettet. Enten man ønsker å hente data fra en nettside, lage automatiserte tester eller integrere databaser og applikasjoner, så kan HTTP-forespørsler være en nyttig løsning.

## Hvordan du gjør det

Det første du må gjøre er å åpne terminalen og skrive `curl` etterfulgt av URLen til nettstedet du ønsker å gjøre en HTTP-forespørsel til. For eksempel:

```Bash
curl https://www.google.com
```

Dette vil returnere all HTML-koden til nettsiden. Du kan også legge til forskjellige flagg for å formatere utdataen eller legge til header-informasjon i forespørselen. For å lagre utdataen i en fil kan du bruke `>` og skrive navnet på filen du vil lagre den som, for eksempel:

```Bash
curl https://www.google.com > google.html
```

Dette vil lagre HTML-koden til nettsiden i en fil som heter "google.html". Du kan også bruke `wget` for å laste ned hele nettsiden, inkludert bilder, filer osv. For eksempel:

```Bash
wget -r -k https://www.google.com
```

Dette vil laste ned hele nettsiden og lagre den i en mappe som heter "www.google.com".

## Dypdykk

Når du sender en HTTP-forespørsel, sendes det en forespørsel til en server, og serveren sender tilbake informasjon eller data som er etterspurt. Dette skjer gjennom en såkalt "request-response" prosess. Det finnes forskjellige metoder for HTTP-forespørsler, som GET, POST, PUT og DELETE, som brukes til å utføre forskjellige handlinger på serveren. Det finnes også forskjellige statuskoder som indikerer om forespørselen var vellykket eller om det skjedde en feil, som for eksempel 200 for vellykket og 404 for ikke funnet.

## Se også

- ["curl"-kommandoen dokumentasjon](https://curl.se/)
- ["wget"-kommandoen dokumentasjon](https://www.gnu.org/software/wget/)
- [HTTP-forespørsler for nybegynnere](https://www.linux.com/training-tutorials/how-handle-http-requests-curl-wget/)
- [Guide til HTTP-metoder og statuskoder](https://www.restapitutorial.com/httpstatuscodes.html)