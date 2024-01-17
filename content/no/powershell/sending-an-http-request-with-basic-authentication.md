---
title:                "Send en http-forespørsel med grunnleggende autentisering"
html_title:           "PowerShell: Send en http-forespørsel med grunnleggende autentisering"
simple_title:         "Send en http-forespørsel med grunnleggende autentisering"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Sending av en HTTP forespørsel med grunnleggende autentisering er en måte for en programmerer å autentisere seg selv når de kommuniserer med en nettbasert tjeneste. Dette er et vanlig krav for å få tilgang til og kommunisere med API-er eller webtjenester. Å bruke grunnleggende autentisering er en enkel og effektiv måte å sikre at informasjonen som utveksles er beskyttet.

## Hvordan:

```PowerShell
# Importer nødvendige moduler:
Import-Module Microsoft.PowerShell.Utility
# Definer variabler for brukernavn og passord:
$user = "brukernavn"
$password = "passord"
# Lag HTTP forespørsel og legg til autentiseringsinformasjon:
Invoke-WebRequest -Uri "https://eksempel.com/api/tjeneste" -Method Get -Headers @{Authorization = "Basic $( [Convert]::ToBase64String([Text.Encoding]:: ASCII.GetBytes("$user:$password")))"
```

*Dette vil være et eksempel på en GET-forespørsel med grunnleggende autentisering. Output vil bli en liste over tilgjengelige ressurser fra webtjenesten.*

## Dypdykk:

Historisk sett var grunnleggende autentisering et av de tidligste autentiseringssystemene brukt på internett. Det er også kjent som Basic Access Authentication og har vært en del av HTTP-standarden siden 1996. Det er en enkel og pålitelig måte å autentisere seg selv ved hjelp av brukernavn og passord.

Det finnes også andre autentiseringssystemer som Digest Access Authentication og OAuth som brukes i tillegg til eller i stedet for grunnleggende autentisering. Disse systemene er mer komplekse, men tilbyr en høyere grad av sikkerhet.

Når du sender en HTTP forespørsel med grunnleggende autentisering, blir brukernavn og passord kodet som en base64-streng og lagt til i forespørselshodene. Denne autentiseringsmetoden kan brukes for både GET og POST-forespørsler.

## Se også:

- [Microsofts dokumentasjon om grunnleggende autentisering i PowerShell](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-7)
- [En kort guide om autentisering med PowerShell](https://www.codementor.io/@team/how-to-use-basic-authentication-in-powershell-iimef1jei)
- [En dypere forklaring av grunnleggende autentisering og fordeler og ulemper med denne autentiseringsmetoden](https://www.varonis.com/blog/basic-authentication/)