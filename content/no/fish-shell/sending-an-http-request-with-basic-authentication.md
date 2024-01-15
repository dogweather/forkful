---
title:                "Sending av en http-forespørsel med grunnleggende autentisering"
html_title:           "Fish Shell: Sending av en http-forespørsel med grunnleggende autentisering"
simple_title:         "Sending av en http-forespørsel med grunnleggende autentisering"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hvorfor

Er du klar til å interagere med internettet på neste nivå? Da bør du lære å sende HTTP-forespørsler med grunnleggende autentisering ved hjelp av Fish Shell. Dette vil hjelpe deg å kommunisere med eksterne servere og hente data fra dem.

## Hvordan

Det første du må gjøre er å installere curl kommandoen hvis du ikke allerede har den på datamaskinen din. Dette kan gjøres ved å åpne terminalen og skrive:

```
Fish Shell installasjon curl
```

Når curl er installert, kan du begynne å sende HTTP-forespørsler med grunnleggende autentisering. Her er et eksempel på hvordan du kan gjøre det:

```
Fish Shell curl -u brukernavn:passord <url>
```

Dette vil sende en GET-forespørsel til den angitte URL-en med brukernavnet og passordet ditt som er kodet i base64-format. Resultatet vil bli vist i terminalen som standard, men du kan også spesifisere en fil å lagre resultatet i med `-o` flagget.

## Dypdykk

Nå som du har lært å sende en grunnleggende autentisert HTTP-forespørsel, kan du utforske flere muligheter. For eksempel kan du bruke forskjellige HTTP-metoder som POST, PUT og DELETE ved å spesifisere dem med `-X` flagget. Du kan også legge til ytterligere parametere og headerinformasjon med `-d` og `-H` flaggene.

Det kan også være nyttig å automatisk hente brukernavn og passord fra omgivelsene ved å bruke `$USER` og `$PASS` variablene. Dette vil bidra til å holde sensitiv informasjon privat og ikke inkludere den i kommandolinjen.

## Se Også

- [Fish Shell officielle dokumentasjon] (https://fishshell.com/docs/current/) 
- [Curl kommandoen dokumentasjon] (https://curl.se/docs/manual.html)