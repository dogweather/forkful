---
title:                "Laddar ner en webbsida"
html_title:           "PowerShell: Laddar ner en webbsida"
simple_title:         "Laddar ner en webbsida"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att ladda ner en webbsida är att hämta dess innehåll från internet och spara det på din dator. Programmerare gör detta för att hämta och bearbeta data från olika webbplatser för att lösa olika problem eller uppgifter.

## Hur man gör:
För att ladda ner en webbsida i PowerShell, använd kommandot "Invoke-WebRequest" följt av webbadressen eller URL:en.

```PowerShell
Invoke-WebRequest www.example.com
```

Detta kommer att hämta innehållet på den angivna webbplatsen och visa resultatet på skärmen, inklusive HTML-koden. Om du vill spara resultatet i en fil kan du använda kommandot "Out-File" följt av en filnamn och dess filtyp.

```PowerShell
Invoke-WebRequest www.example.com | Out-File example.html
```

Nu kommer resultatet att sparas i en HTML-fil på din dator. För att snygga upp resultatet och visa det endast som text, kan du använda "ConvertTo-HTML" kommandot.

```PowerShell
Invoke-WebRequest www.example.com | ConvertTo-HTML -As String
```

Detta kommer att konvertera resultatet till vanlig text och visa det på skärmen. Du kan också använda "Out-File" för att spara den konverterade texten i en fil.

## Djupdykning:
Historiskt sett så har det funnits andra sätt att ladda ner webbsidor, såsom att använda speciella program eller skript. Men med PowerShell kan du sömlöst utföra detta utan att behöva lämna det digitala ekosystemet. Du kan också använda PowerShell för att automatisera processen med att ladda ner och bearbeta webbdata för att effektivisera ditt arbete.

## Se även:
Läs mer om Invoke-WebRequest på [Microsofts dokumentationssida](https://docs.microsoft.com/en-us/powershell/module/Microsoft.PowerShell.Utility/Invoke-WebRequest). Du kan också utforska andra sätt att hämta webbdata med PowerShell på [PowerShell Gallery](https://www.powershellgallery.com/packages?q=web+request).