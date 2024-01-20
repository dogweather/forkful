---
title:                "Analisi dell'HTML"
date:                  2024-01-20T15:33:11.276961-07:00
html_title:           "Bash: Analisi dell'HTML"
simple_title:         "Analisi dell'HTML"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/powershell/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa & Perché?)
Il parsing di HTML consiste nell'estrarre dati da una pagina web. I programmatori lo fanno per automatizzare il recupero di informazioni, integrare contenuti esterni nelle app o per analisi di dati web.

## How to: (Come fare:)
Per fare il parsing di HTML in PowerShell, puoi utilizzare il cmdlet `Invoke-WebRequest` e il pacchetto HtmlAgilityPack. Esempio:

```PowerShell
# Installa il pacchetto HtmlAgilityPack se non presente
# Install-Package HtmlAgilityPack

# Usa Invoke-WebRequest e seleziona elementi HTML
$response = Invoke-WebRequest -Uri 'http://example.com'
$html = New-Object -TypeName HtmlAgilityPack.HtmlDocument
$html.LoadHtml($response.Content)

# Estrai dati usando XPath
$nodes = $html.DocumentNode.SelectNodes('//h1')
foreach ($node in $nodes) {
    Write-Output $node.InnerText
}
```
Output di esempio:
```
Titolo della Pagina
```

## Deep Dive (Analisi Approfondita)
Il parsing di HTML risale agli albori del web per l'analisi dei contenuti. PowerShell supporta varie tecniche incluse regex e packages come HtmlAgilityPack. Regex è rapido ma può essere impreciso. HtmlAgilityPack è uno standard de facto, affidabile e flessibile, che imita il DOM e comprende XPath e CSS selectors.

## See Also (Vedi Anche)
- [HtmlAgilityPack su GitHub](https://github.com/zzzprojects/html-agility-pack)
- [Documentazione ufficiale PowerShell](https://docs.microsoft.com/it-it/powershell/)
- [XPath Tutorial](https://www.w3schools.com/xml/xpath_intro.asp)