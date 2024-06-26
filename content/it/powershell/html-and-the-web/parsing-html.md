---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:42.232453-07:00
description: "Come fare: PowerShell non ha nativamente un parser HTML dedicato, ma\
  \ \xE8 possibile utilizzare il cmdlet `Invoke-WebRequest` per accedere e analizzare\
  \ il\u2026"
lastmod: '2024-03-13T22:44:43.640663-06:00'
model: gpt-4-0125-preview
summary: "PowerShell non ha nativamente un parser HTML dedicato, ma \xE8 possibile\
  \ utilizzare il cmdlet `Invoke-WebRequest` per accedere e analizzare il contenuto\
  \ HTML."
title: Analisi del HTML
weight: 43
---

## Come fare:
PowerShell non ha nativamente un parser HTML dedicato, ma è possibile utilizzare il cmdlet `Invoke-WebRequest` per accedere e analizzare il contenuto HTML. Per un parsing e una manipolazione più complessi, si può impiegare HtmlAgilityPack, una popolare libreria .NET.

### Utilizzando `Invoke-WebRequest`:
```powershell
# Esempio semplice per recuperare i titoli da una pagina web
$response = Invoke-WebRequest -Uri 'http://example.com'
# Utilizzare la proprietà ParsedHtml per accedere agli elementi del DOM
$title = $response.ParsedHtml.title
Write-Output $title
```

Output di esempio:

```
Example Domain
```

### Utilizzando HtmlAgilityPack:
Prima, è necessario installare l'HtmlAgilityPack. È possibile farlo tramite NuGet Package Manager:

```powershell
Install-Package HtmlAgilityPack -ProviderName NuGet
```

Poi, è possibile utilizzarlo in PowerShell per analizzare l'HTML:

```powershell
# Caricare l'assembly di HtmlAgilityPack
Add-Type -Path "path\to\HtmlAgilityPack.dll"

# Creare un oggetto HtmlDocument
$doc = New-Object HtmlAgilityPack.HtmlDocument

# Caricare HTML da un file o da una richiesta web
$htmlContent = (Invoke-WebRequest -Uri "http://example.com").Content
$doc.LoadHtml($htmlContent)

# Utilizzare XPath o altri metodi di query per estrarre elementi
$node = $doc.DocumentNode.SelectSingleNode("//h1")

if ($node -ne $null) {
    Write-Output $node.InnerText
}
```

Output di esempio:

```
Benvenuti su Example.com!
```

In questi esempi, `Invoke-WebRequest` è migliore per compiti semplici, mentre HtmlAgilityPack offre un insieme di funzionalità molto più ricco per parsing e manipolazione HTML complessi.
