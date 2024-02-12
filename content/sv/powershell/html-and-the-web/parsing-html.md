---
title:                "Tolka HTML"
date:                  2024-02-03T19:13:00.178103-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tolka HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?
Att analysera HTML med PowerShell handlar om att dissekera HTML-innehåll för att extrahera specifika data eller automatisera webbrelaterade uppgifter. Programmerare gör det för att interagera med webbsidor, skrapa webbinnehåll, eller automatisera formulärinlämningar och andra webbinteraktioner utan att behöva en webbläsare.

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
