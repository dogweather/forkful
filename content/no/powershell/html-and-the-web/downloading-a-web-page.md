---
date: 2024-01-20 17:44:30.405237-07:00
description: 'How to: I PowerShell kan du laste ned en nettside med `Invoke-WebRequest`.
  Sjekk ut dette eksemplet.'
lastmod: '2024-03-13T22:44:41.016647-06:00'
model: gpt-4-1106-preview
summary: I PowerShell kan du laste ned en nettside med `Invoke-WebRequest`.
title: Nedlasting av en nettside
weight: 42
---

## How to:
I PowerShell kan du laste ned en nettside med `Invoke-WebRequest`. Sjekk ut dette eksemplet:

```PowerShell
$response = Invoke-WebRequest -Uri 'https://example.com'
$response.Content > 'example-page.html'
```

Kjører du dette, får du HTML-innholdet i 'example.com' og lagrer det i en fil kalt 'example-page.html'.

## Deep Dive
Tilbake i tiden brukte vi `System.Net.WebClient` klassen i .NET Framework for å laste ned nettsider, men PowerShell forenklet prosessen med `Invoke-WebRequest`. Denne kommandoen er mer enn bare nedlasting; den håndterer også sesjonsstyring, og kan etterligne nettleserinteraksjoner med serveren. 

Alternativer inkluderer `Invoke-RestMethod` for API-endepunkter som returnerer JSON eller XML, eller tredjepartsverktøy som `curl`. Intern fungerer `Invoke-WebRequest` ved først å etablere en HTTP-forbindelse til serveren og deretter å sende en GET-forespørsel for å hente innholdet. 

Prosessen kan bli mer avansert når du trenger å håndtere cookies, hoderverdier eller POST-forespørslers data.

## See Also
- Microsofts dokumentasjon for `Invoke-WebRequest`: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest
- En guide til hvordan du bruker `Invoke-RestMethod`: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-restmethod
- `curl` offisielle nettside: https://curl.se/
