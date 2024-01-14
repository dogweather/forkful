---
title:                "Bash: Sende en http-forespørsel med grunnleggende autentisering"
simple_title:         "Sende en http-forespørsel med grunnleggende autentisering"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hvorfor

Å sende en HTTP forespørsel med grunnleggende autentisering kan være nyttig når du ønsker å få tilgang til et ressurs som krever autentisering via et brukernavn og passord. Dette kan for eksempel være å hente data fra en API eller å logge inn på en nettside.

## Hvordan

Det er enkelt å sende en HTTP forespørsel med grunnleggende autentisering ved hjelp av Bash programmering. Følg disse trinnene for å implementere det:

1. Lag et curl-kommando som inkluderer brukernavn og passord i en form av "brukernavn:passord". Merk at dette er base64-kodet, så du må konvertere brukernavn og passord til base64-format først. Eksempel: ```curl --user "dittBrukernavn:dittPassord"```
2. Legg til URL-en du vil sende forespørselen til. Eksempel: ```-X GET https://api.example.com```
3. Kjør kommandoen og se på outputen for å være sikker på at autentiseringen er vellykket. Outputen bør vise data fra forespørselen hvis alt gikk greit.

Et eksempel på hvordan en slik curl-kommando kan se ut i sin helhet:

```Bash
curl --user "dittBrukernavn:dittPassord" -X GET https://api.example.com
```

## Dypdykk

For å forstå hvordan denne prosessen fungerer dypere, er det viktig å vite at grunnleggende autentisering er en enkel metode for autentisering som er basert på brukernavn og passord. Når du sender en HTTP forespørsel med en autentiseringsheader, blir dette informasjonen base64-kodet før det sendes til serveren. Serveren vil da dekode denne informasjonen og validere om brukernavn og passord er riktig.

En annen viktig ting å merke seg er at grunnleggende autentisering ikke krypterer informasjonen som sendes, så det er ikke en veldig sikker metode for autentisering. Det anbefales å bruke andre metoder som OAuth eller API nøkler for å sikre autentiseringen.

## Se også

- [Bash curl dokumentasjon](https://curl.se/docs/manpage.html)
- [OAuth autentisering](https://oauth.net/)
- [Sikker autentisering](https://www.owasp.org/index.php/Authentication_Cheat_Sheet)