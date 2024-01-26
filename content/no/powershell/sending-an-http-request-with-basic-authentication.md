---
title:                "Å sende en HTTP-forespørsel med grunnleggende autentisering"
date:                  2024-01-20T18:02:33.143450-07:00
model:                 gpt-4-1106-preview
simple_title:         "Å sende en HTTP-forespørsel med grunnleggende autentisering"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
En HTTP-forespørsel med grunnleggende autentisering innebærer å sende brukernavn og passord over nett for å verifisere en brukers identitet. Programmerere gjør det for å sikre tilgang til API-er eller webressurser som krever identifikasjon.

## Hvordan:
For å gjøre en HTTP-forespørsel med grunnleggende autentisering i PowerShell, bruker du `Invoke-RestMethod` eller `Invoke-WebRequest`. Her er et enkelt eksempel:

```PowerShell
# Opprett autentiseringsinformasjonen
$brukernavn = 'mittBrukernavn'
$passord = 'mittPassord'

# Konverter til Base64-streng
$Base64AuthInfo = [Convert]::ToBase64String([Text.Encoding]::ASCII.GetBytes("$($brukernavn):$($passord)"))

# Lag en HTTP-headers med autorisasjonsfeltet
$headers = @{
    Authorization = "Basic $Base64AuthInfo"
}

# Send GET-forespørselen
$url = 'https://mittapi.no/endepunkt'
$response = Invoke-RestMethod -Uri $url -Method Get -Headers $headers

# Skriv ut svaret
$response
```

Du vil få et responsobjekt som inneholder dataene som API-et returnerte.

## Dykk dypere:
I 1996, da HTTP 1.0-spesifikasjonen (RFC 1945) ble utviklet, var Basic Authentication allerede en del av det foreløpige utkastet. Til tross for at det ikke er den sikreste autentiseringsmetoden, siden det sender legitimasjon i klartekst (bare base64-kodet), er det fortsatt i bruk på grunn av dens enkelhet. Flere alternativer som OAuth, tokens, eller API-nøkler er ofte bedre for sikkerhet, men grunnleggende autentisering kan være nyttig for interne eller lavrisikotjenester hvor enkel tilgang er prioritert.

Når du implemeneterer dette i PowerShell, er det viktig å kryptere trafikken ved hjelp av HTTPS for å forhindre at legitimasjonen blir avlyttet. PowerShell forenkler prosessen ved å tillate at du legger til `Authorization`-headeren direkte i forespørselen.

## Se også:
- [PowerShell dokumentasjon for `Invoke-RestMethod`](https://docs.microsoft.com/powershell/module/microsoft.powershell.utility/invoke-restmethod)
- [RFC 7617 'The 'Basic' HTTP Authentication Scheme'](https://tools.ietf.org/html/rfc7617)
- [Guide til sikrere autentiseringsmetoder (`OAuth 2.0`)](https://oauth.net/2/)
