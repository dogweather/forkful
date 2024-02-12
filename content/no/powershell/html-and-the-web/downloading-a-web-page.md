---
title:                "Nedlasting av en nettside"
aliases:
- /no/powershell/downloading-a-web-page/
date:                  2024-01-20T17:44:30.405237-07:00
model:                 gpt-4-1106-preview
simple_title:         "Nedlasting av en nettside"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?
Nedlasting av en nettside betyr å hente kildekoden for siden til din datamaskin. Programmerere gjør dette for å analysere innholdet, automatisere datainnsamling eller sjekke tilgjengeligheten til en nettside.

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
