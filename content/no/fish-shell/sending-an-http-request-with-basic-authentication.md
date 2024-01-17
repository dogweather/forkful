---
title:                "Sending av http-forespørsel med grunnleggende autentisering"
html_title:           "Fish Shell: Sending av http-forespørsel med grunnleggende autentisering"
simple_title:         "Sending av http-forespørsel med grunnleggende autentisering"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å sende en HTTP-forespørsel med basic authentication betyr at du kan autentisere deg selv til en server ved å bruke brukernavnet og passordet ditt. Dette er nyttig for å få tilgang til beskyttede ressurser og for å sikre kommunikasjon mellom klient og server.

## Hvordan:
Fish Shell gjør det enkelt å sende en HTTP-forespørsel med basic authentication. Du kan bruke kommandoen ```curl``` og følge syntaksen nedenfor for å sende en forespørsel med en header for basic authentication:

```
curl -u <brukernavn>:<passord> <URL>
```

For eksempel, hvis du ønsker å hente data fra et API som krever basic authentication kan du bruke følgende kommando:

```
curl -u john:password123 https://api.example.com/data
```

Dette vil sende en GET-forespørsel med autentiseringen til API-et og hente ut dataene.

## Dykke dypere:
HTTP-forespørsler med basic authentication har vært en vanlig metode for å autentisere brukere siden starten av HTTP-protokollen. Alternativer til dette inkluderer OAuth, som gir bedre sikkerhet og brukeropplevelse, men er mer komplisert å implementere.

Når det gjelder implementering, er det viktig å sørge for å bruke HTTPS i stedet for HTTP for å sikre at brukernavn og passord ikke blir sendt som klartekst. I tillegg bør man vurdere å bruke tofaktorautentisering for å styrke sikkerheten.

## Se også:
- [Fish Shell documentation](https://fishshell.com/docs/current/index.html)
- [HTTP Authentication: Basic and Digest Access Authentication](https://en.wikipedia.org/wiki/Basic_access_authentication)