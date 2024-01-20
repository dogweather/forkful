---
title:                "Ladda ner en webbsida"
html_title:           "Bash: Ladda ner en webbsida"
simple_title:         "Ladda ner en webbsida"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att ladda ner en webbsida innebär att få tillgång till och spara dess HTML-kod till ett lokalt system. Programmerare gör detta för att analysera eller manipulera webbsidans data.

## Hur man gör:

Här är ett grundläggande exempel med PowerShell:

```PowerShell
$pageUrl = 'https://www.example.com'
$webClient = New-Object System.Net.WebClient
$pageData = $webClient.DownloadString($pageUrl)
$pageData
```

Utför den här koden så laddas HTML-koden för 'https://www.example.com' ner och visas.

## Djupdykning 

Hämtning av webbsidor är inget nytt, redan från tidigt 90-tal har utvecklare hämtat webbsidor för att analysera dess data. Det finns också alternativ till PowerShell för detta, exempelvis Python, Java eller Curl.

På låg nivå använder PowerShell .NET-biblioteket System.Net.WebClient för att utföra HTTP-förfrågan och få svaret. Dess 'DownloadString' metod används för att hämta HTML-svaret som en sträng.

## Se Även 

Du kan lära dig mer om PowerShell med dessa länkar:

1. Officiella Microsoft PowerShell Documentation: https://docs.microsoft.com/en-us/powershell/
2. WebClient .NET Class: https://docs.microsoft.com/en-us/dotnet/api/system.net.webclient?view=netframework-4.8
3. HTTP Protocol: https://developer.mozilla.org/en-US/docs/Web/HTTP