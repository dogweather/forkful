---
title:                "Å sende en http-forespørsel"
html_title:           "C++: Å sende en http-forespørsel"
simple_title:         "Å sende en http-forespørsel"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å sende en HTTP-forespørsel er en metode å kommunisere med webtjenester og servere på. Programmere gjør dette for å hente, legge til, eller endre data på internett.

## Slik gjør du:

I PowerShell, bruker du `Invoke-RestMethod` eller `Invoke-WebRequest` for å sende HTTP-forespørsler. Her er enkelteksempler.

```PowerShell
# Hente data med GET request
$response = Invoke-WebRequest -Uri "http://api.example.com/data"

# Send data med POST request
$body = @{key1='value1';key2='value2'}
$response = Invoke-WebRequest -Uri "http://api.example.com/data" -Method POST -Body $body
```
Output vil være som følger:

```PowerShell
$response.Content
```

## Dypdykk:

Historisk sett, er HTTP-forespørsler implantert i mange programmeringsspråk og verktøy, ikke bare PowerShell. Alternativer til `Invoke-RestMethod` og `Invoke-WebRequest` i PowerShell kan være `curl` og `wget` i Unix/Linux-baserte systemer, eller `HttpClient` klasse i C#.

Når du utfører `Invoke-WebRequest` i PowerShell, bruker det `HttpWebRequest` klassen under hetten, som er en del av .NET rammeverket.

## Se også:

- [Microsoft Offisiell Dokumentasjon for Invoke-WebRequest](https://docs.microsoft.com/nb-no/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-7.1)
- [Microsoft Offisiell Dokumentasjon for Invoke-RestMethod](https://docs.microsoft.com/nb-no/powershell/module/microsoft.powershell.utility/invoke-restmethod?view=powershell-7.1)
- [Wiki om HTTP Protokoll](https://no.wikipedia.org/wiki/Hypertext_Transfer_Protocol)