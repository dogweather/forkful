---
date: 2024-01-20 18:00:21.709792-07:00
description: "Slik gj\xF8r du: For \xE5 sende en HTTP GET-foresp\xF8rsel i PowerShell\
  \ og vise svaret."
lastmod: '2024-03-13T22:44:41.014869-06:00'
model: gpt-4-1106-preview
summary: "For \xE5 sende en HTTP GET-foresp\xF8rsel i PowerShell og vise svaret."
title: "\xC5 sende en HTTP-foresp\xF8rsel"
weight: 44
---

## Slik gjør du:
For å sende en HTTP GET-forespørsel i PowerShell og vise svaret:

```PowerShell
$response = Invoke-RestMethod -Uri 'http://example.com/api/data' -Method Get
Write-Output $response
```

For å sende en HTTP POST-forespørsel med JSON-innhold:

```PowerShell
$body = @{
    key1 = 'value1'
    key2 = 'value2'
} | ConvertTo-Json

$response = Invoke-RestMethod -Uri 'http://example.com/api/data' -Method Post -Body $body -ContentType 'application/json'
Write-Output $response
```

Eksempeloutput:

```
ID       Name       Job
--       ----       ---
101      John Doe   Developer
102      Jane Smith Manager
```

## Dykk dypere
Sending av HTTP-forespørsler har vært essensielt siden nettets begynnelse for å tillate client-server-kommunikasjon. I PowerShell var det i starten vanlig å bruke `WebRequest` og `WebResponse` objekter før `Invoke-WebRequest` og `Invoke-RestMethod` cmdletene ble introdusert, noe som simplified prosessen betydelig.

Alternativt til PowerShell kan man bruke cURL, som er et kommandolinjeverktøy, eller programmeringsspråk som Python, Ruby, eller JavaScript for å sende HTTP-forespørsler.

Implementasjonsdetaljer:
- `Invoke-RestMethod` er ofte brukt for API-interaksjon siden den håndterer JSON og XML på en smidig måte.
- Det er viktig å behandle HTTP-statuskoder og feilhåndtering effektivt. Bruk `-ErrorAction` og try/catch blokker for dette.
- For autentisering, bruk `-Credential` eller manipuler `Headers` for å inkludere tokens.

## Se også
- [PowerShell Documentation on Invoke-RestMethod](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-restmethod)
- [About Status Codes (HTTP Cats)](https://http.cat/)
- [cURL](https://curl.se/)
