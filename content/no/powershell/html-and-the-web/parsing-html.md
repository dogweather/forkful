---
aliases:
- /no/powershell/parsing-html/
date: 2024-01-20 15:33:10.166240-07:00
description: "Parsing av HTML handler om \xE5 tolke og trekke ut data fra HTML-koder.\
  \ Programmerere gj\xF8r dette for \xE5 automatisere prosesser, h\xF8ste data eller\
  \ integrere\u2026"
lastmod: 2024-02-18 23:08:54.098359
summary: "Parsing av HTML handler om \xE5 tolke og trekke ut data fra HTML-koder.\
  \ Programmerere gj\xF8r dette for \xE5 automatisere prosesser, h\xF8ste data eller\
  \ integrere\u2026"
title: Analyse av HTML
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Parsing av HTML handler om å tolke og trekke ut data fra HTML-koder. Programmerere gjør dette for å automatisere prosesser, høste data eller integrere nettsider i apps.

## Hvordan:
```PowerShell
# Installer nødvendig modul
Install-Module -Name HtmlAgilityPack

# Last inn HtmlAgilityPack
Add-Type -Path "C:\path\to\HtmlAgilityPack.dll"

# Last inn HTML-dokumentet
$html = New-Object HtmlAgilityPack.HtmlDocument
$html.LoadHtml((Invoke-WebRequest -Uri "https://eksempelside.no").Content)

# Finn elementer ved bruk av XPath
$nodes = $html.DocumentNode.SelectNodes('//h1')

# Skriv ut resultatet
$nodes | ForEach-Object { $_.InnerText }
```
Eksempelresultat:
```
Velkommen til vår hjemmeside!
```

## Dypdykk
Å tolke HTML har vokst ut fra behovet for å forstå og manipulere webinnhold. Web skraping er en vanlig bruk, men det bryter ofte med nettsidens brukervilkår, så vær forsiktig. Alternativer til HtmlAgilityPack inkluderer AngleSharp og .NET sin egen `HttpClient` klasse for nettanrop kombinert med regulære uttrykk, selv om sistnevnte kan være feilutsatt. For mer robuste løsninger er kunnskap om DOM (Document Object Model) og bruk av XPath eller CSS-selectors viktig.

## Se Også
- HtmlAgilityPack dokumentasjon: https://html-agility-pack.net/
- Web scraping guide: https://www.scrapehero.com/how-to-scrape-websites-with-powershell/
- XPath Syntax: https://www.w3schools.com/xml/xpath_syntax.asp
