---
title:                "Sende en http-forespørsel med grunnleggende autentisering"
html_title:           "Kotlin: Sende en http-forespørsel med grunnleggende autentisering"
simple_title:         "Sende en http-forespørsel med grunnleggende autentisering"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å sende en HTTP-forespørsel med grunnleggende godkjenning er prosessen hvor programmene dine, ved hjelp av brukernavn og passord, får tilgang til beskyttede serverressurser. Dette er nyttig slik at programmene dine kan interagere trygt og effektivt med andre systemer.

## Hvordan:

La oss ta en titt på et grunnleggende eksempel på en PowerShell-skript for dette. For å sende en HTTP-forespørsel med grunnleggende godkjenning i PowerShell, bruk følgende kode:

```PowerShell
# Opprett en ny variabel med URL til serveren du vil hente data fra
$url = 'https://min.server.com'

# Definer brukernavn og passord
$username = 'brukernavn'
$password = 'passord'

# Konverter brukernavn og passord til base64
$base64AuthInfo = [Convert]::ToBase64String([Text.Encoding]::ASCII.GetBytes(("${username}:${password}")))

# Send forespørsel til serveren
$response = Invoke-WebRequest -Headers @{Authorization=("Basic {0}" -f $base64AuthInfo)} -Uri $url

# Utskrift av respons
$response
```
Denne koden vil gi deg en respons fra serveren.

## Dypdykk:

La oss nå dykke dypere inn i dette.

(1) Historisk kontekst: Grunnleggende godkjenning er en del av HTTP/1.0-standarden, som ble introdusert tilbake i 1996. Det tillater klienter å bevise deres identitet til serveren ved å dele en brukernavn-passord-kombinasjon.

(2) Alternativer: Selv om grunnleggende godkjenning er enkel å implementere, er det ikke den sikreste metoden for godkjenning. Alternativer inkluderer Digest Authentication og OAuth.

(3) Implementasjonsinformasjon: Grunnleggende godkjenning er enkel å bruke, men er ikke sikker fordi den bruker base64-koding, som lett kan dekodes. For å øke sikkerheten, bør det brukes sammen med HTTPS for å kryptere dataene som sendes over nettverket.

## Se også:

For mer detaljert informasjon kan du sjekke ut følgende ressurser:

- [Microsoft - Invoke-WebRequest](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest)

- [Wikipedia - Basic access authentication](https://en.wikipedia.org/wiki/Basic_access_authentication)

- [Microsoft - Connect to Exchange servers using remote PowerShell](https://docs.microsoft.com/en-us/powershell/exchange/connect-to-exchange-servers-using-remote-powershell)