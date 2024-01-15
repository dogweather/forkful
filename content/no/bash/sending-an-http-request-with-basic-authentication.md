---
title:                "Sending en Http-forespørsel med grunnleggende autentisering"
html_title:           "Bash: Sending en Http-forespørsel med grunnleggende autentisering"
simple_title:         "Sending en Http-forespørsel med grunnleggende autentisering"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du noen gang har lurt på hvordan du kanskje kan få tilgang til et passordbeskyttet nettsted ved hjelp av et enkelt kommandolinjeverktøy, eller automatisere en prosess med å sende informasjon til et nettsted via HTTP, så er dette artikkelen for deg. Basic authentication er en enkel måte å sende HTTP-forespørsler med godkjenning, og ved å bruke Bash kan du enkelt implementere dette i dine egne prosjekter.

## Hvordan

For å sende en HTTP-forespørsel med basic authentication i Bash, må du først sette opp variabler for din brukernavn og passord:

```Bash
username="brukernavn"
password="passord"
```

Deretter kan du bruke curl-kommandoen til å sende en GET-forespørsel til et nettsted med basic authentication:

```Bash
curl --user "$username:$password" www.eksempelnettsted.com
```

Dette vil returnere en respons fra nettstedet som du kan viderebehandle eller lagre i en variabel for senere bruk.

## Deep Dive

Basic authentication er en metode for godkjenning i HTTP-protokollen ved å sende brukernavn og passord i klartekst via HTTP-headerene. Dette er en enkel og vanlig måte å beskytte nettstedsressurser på, men er ikke en like sikker løsning som HTTPS. Derfor bør du kun bruke basic authentication hvis du er trygg på at informasjonen som sendes via HTTP er ikke-kritisk.

Det er også viktig å merke seg at basic authentication ikke gir noen form for kryptering eller beskyttelse mot man-in-the-middle-angrep. Det er derfor viktig å vurdere bruken av HTTPS i tillegg hvis du trenger ekstra sikkerhet for dine HTTP-forespørsler.

## Se også 

- [Curl Documentation](https://curl.haxx.se/docs/) 
- [HTTP Basic Authentication Explained](https://medium.com/@nishant6167/http-basic-authentication-explained-c4c06206777e) 
- [Bash scripting cheatsheet](https://devhints.io/bash)