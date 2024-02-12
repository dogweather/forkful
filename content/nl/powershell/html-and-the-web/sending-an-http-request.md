---
title:                "Een HTTP-verzoek verzenden"
aliases:
- /nl/powershell/sending-an-http-request/
date:                  2024-01-28T22:07:44.034481-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een HTTP-verzoek verzenden"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/powershell/sending-an-http-request.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Een HTTP-verzoek versturen is hoe je om gegevens of een reactie van een webdienst vraagt. Programmeurs doen dit om te communiceren met API's, website-inhoud te fetchen, of om te communiceren met externe servers.

## Hoe:

Hier is de duidelijke methode om een eenvoudig GET-verzoek af te vuren:

```PowerShell
$response = Invoke-RestMethod -Uri 'https://api.example.com/data' -Method Get
Write-Output $response
```

En, als je wat informatie wilt POSTen:

```PowerShell
$body = @{
    'name' = 'Jane Doe'
    'occupation' = 'Ruimteranger'
}

$response = Invoke-RestMethod -Uri 'https://api.example.com/users' -Method Post -Body ($body | ConvertTo-Json)
Write-Output $response
```

Voorbeelduitvoer:

```
name         occupation
----         ----------
Jane Doe     Ruimteranger
```

## Diepgaand:

Een HTTP-verzoek versturen gaat terug tot de dageraad van webontwikkeling. Je engageert in een dialoog met het web in zijn moedertaal, HTTP. PowerShell's `Invoke-RestMethod` cmdlet is hier het gereedschap bij uitstek. Voor `Invoke-RestMethod`, was `Invoke-WebRequest` de go-to, en het is er nog steeds voor meer gedetailleerde reacties.

Je hebt alternatieven zoals `curl` of .NET's `HttpClient` class als je avontuurlijk bent ingesteld. Wanneer je `Invoke-RestMethod` gebruikt, bedenk dan dat het een wrapper is rondom .NET's `HttpClient` klassen en methodes, die eenvoud biedt maar enige lage-niveau controle opoffert.

Wat implementatie betreft, onthoud dat HTTP-verzoeken komen met verschillende methoden zoals `GET`, `POST`, `PUT`, etc. Pas headers aan met `-Headers`, en beheer time-outs en authenticatie met extra parameters naar behoefte. Saniteer altijd inputs als je gebruikmaakt van door gebruikers gegenereerde inhoud om injectieaanvallen te voorkomen.

## Zie Ook:

- [Over PowerShell's Invoke-RestMethod](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-restmethod)
- [`Invoke-WebRequest` details](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest)
- [Begrip van REST API's](https://www.redhat.com/en/topics/api/what-is-a-rest-api)
- [`.NET HttpClient` Klasse](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient)
