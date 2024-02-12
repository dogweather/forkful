---
title:                "Analyse av HTML"
aliases: - /no/powershell/parsing-html.md
date:                  2024-01-20T15:33:10.166240-07:00
simple_title:         "Analyse av HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/parsing-html.md"
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
