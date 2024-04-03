---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:00.178103-07:00
description: "Hur: PowerShell har inte fr\xE5n b\xF6rjan en dedikerad HTML-parser,\
  \ men du kan anv\xE4nda `Invoke-WebRequest`-cmdleten f\xF6r att komma \xE5t och\
  \ analysera HTML-\u2026"
lastmod: '2024-03-13T22:44:38.123871-06:00'
model: gpt-4-0125-preview
summary: "PowerShell har inte fr\xE5n b\xF6rjan en dedikerad HTML-parser, men du kan\
  \ anv\xE4nda `Invoke-WebRequest`-cmdleten f\xF6r att komma \xE5t och analysera HTML-inneh\xE5\
  ll."
title: Tolka HTML
weight: 43
---

## Hur:
PowerShell har inte från början en dedikerad HTML-parser, men du kan använda `Invoke-WebRequest`-cmdleten för att komma åt och analysera HTML-innehåll. För mer komplex analys och manipulation kan HtmlAgilityPack, ett populärt .NET-bibliotek, användas.

### Använda `Invoke-WebRequest`:
```powershell
# Enkelt exempel för att hämta titlar från en webbsida
$response = Invoke-WebRequest -Uri 'http://example.com'
# Använd egenskapen ParsedHtml för att komma åt DOM-element
$title = $response.ParsedHtml.title
Write-Output $title
```

Exempel på utdata:

```
Example Domain
```

### Använda HtmlAgilityPack:
Först måste du installera HtmlAgilityPack. Detta kan du göra via NuGet Package Manager:

```powershell
Install-Package HtmlAgilityPack -ProviderName NuGet
```

Sedan kan du använda det i PowerShell för att analysera HTML:

```powershell
# Ladda HtmlAgilityPack-assembly
Add-Type -Path "path\to\HtmlAgilityPack.dll"

# Skapa ett HtmlDocument-objekt
$doc = New-Object HtmlAgilityPack.HtmlDocument

# Ladda HTML från en fil eller en webbförfrågan
$htmlContent = (Invoke-WebRequest -Uri "http://example.com").Content
$doc.LoadHtml($htmlContent)

# Använd XPath eller andra frågemetoder för att extrahera element
$node = $doc.DocumentNode.SelectSingleNode("//h1")

if ($node -ne $null) {
    Write-Output $node.InnerText
}
```

Exempel på utdata:

```
Välkommen till Example.com!
```

I dessa exempel är `Invoke-WebRequest` bäst för enkla uppgifter, medan HtmlAgilityPack erbjuder ett mycket rikare funktionssätt för komplex HTML-analys och manipulation.
