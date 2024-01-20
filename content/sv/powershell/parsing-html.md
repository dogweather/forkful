---
title:                "Analysera html"
html_title:           "Arduino: Analysera html"
simple_title:         "Analysera html"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/parsing-html.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att analysera HTML innebär att bryta ner HTML-koden till dess beståndsdelar för att sedan använda dessa delar inom programmeringen. Programmörer gör det för att fånga data, manipulera sidor och automatisera uppgifter webbsidors nivå.

## Hur man gör:

Här är ett enkelt exempel på hur man använder PowerShell för att analysera HTML:

```PowerShell
# Ladda ner en webbsida
$webcontent = Invoke-WebRequest -Uri "https://www.exempelsida.se"

# Analysera HTML:n, extrahera data
$data = $webcontent.ParsedHtml.getElementsByTagName('tagname')

# Skriv ut data
$data | ForEach-Object { Write-Output $_.innerText }
```
I det här exemplet byter du ut "https://www.exempelsida.se" mot webbadressen du vill analysera, och 'tagname' till den HTML-tag du letar efter. Utdata varierar beroende på vilken webbsida och tag du valt.

## Fördjupning:

Historiskt sett har HTML-analys varit komplicerad och beroende av specifika bibliotek. PowerShell förenklar det här mycket genom att innehålla inbyggda metoder för att hämta och analysera webbsidor.

Ett alternativ till Invoke-WebRequest är att använda .NET-klassen WebClient, men det är mer komplicerat och kräver mer kod.

Om du behöver mer kontroll över analysprocessen, eller om du hanterar komplicerad HTML, kan det vara värt att titta på mer avancerade verktyg, som t ex. HtmlAgilityPack.

Powershell utför analysen genom COM-objektet MSHTML, vilket innebär att analyserna i grunden är beroende av Internet Explorer's rendering och kan därför ge olika resultat beroende på vilken version av IE som är installerad på maskinen.

## Länkar till vidare läsning:

1. Powershell dokumentation på [Invoke-WebRequest](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-7.1)
3. Officiella [HTML5-specifikationen](https://www.w3.org/TR/html5/) är oumbärliga om du vill djupdyka in i HTML-analys.