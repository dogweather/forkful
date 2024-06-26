---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:35.163566-07:00
description: "Wie geht das: PowerShell verf\xFCgt nativ nicht \xFCber einen dedizierten\
  \ HTML-Parser, aber Sie k\xF6nnen das Cmdlet `Invoke-WebRequest` verwenden, um auf\
  \ HTML-\u2026"
lastmod: '2024-03-13T22:44:54.101568-06:00'
model: gpt-4-0125-preview
summary: "PowerShell verf\xFCgt nativ nicht \xFCber einen dedizierten HTML-Parser,\
  \ aber Sie k\xF6nnen das Cmdlet `Invoke-WebRequest` verwenden, um auf HTML-Inhalte\
  \ zuzugreifen und diese zu parsen."
title: HTML parsen
weight: 43
---

## Wie geht das:
PowerShell verfügt nativ nicht über einen dedizierten HTML-Parser, aber Sie können das Cmdlet `Invoke-WebRequest` verwenden, um auf HTML-Inhalte zuzugreifen und diese zu parsen. Für komplexeres Parsen und Manipulieren kann der HtmlAgilityPack, eine beliebte .NET-Bibliothek, verwendet werden.

### Verwendung von `Invoke-WebRequest`:
```powershell
# Einfaches Beispiel, um Titel von einer Webseite zu holen
$response = Invoke-WebRequest -Uri 'http://example.com'
# Die ParsedHtml-Eigenschaft nutzen, um auf DOM-Elemente zuzugreifen
$title = $response.ParsedHtml.title
Write-Output $title
```

Beispielausgabe:

```
Example Domain
```

### Verwendung von HtmlAgilityPack:
Zuerst müssen Sie den HtmlAgilityPack installieren. Das können Sie über den NuGet Package Manager tun:

```powershell
Install-Package HtmlAgilityPack -ProviderName NuGet
```

Dann können Sie ihn in PowerShell verwenden, um HTML zu parsen:

```powershell
# Das HtmlAgilityPack-Assembly laden
Add-Type -Path "Pfad\zum\HtmlAgilityPack.dll"

# Ein HtmlDocument-Objekt erstellen
$doc = New-Object HtmlAgilityPack.HtmlDocument

# HTML aus einer Datei oder einer Webanfrage laden
$htmlContent = (Invoke-WebRequest -Uri "http://example.com").Content
$doc.LoadHtml($htmlContent)

# XPath oder andere Abfragemethoden nutzen, um Elemente zu extrahieren
$node = $doc.DocumentNode.SelectSingleNode("//h1")

if ($node -ne $null) {
    Write-Output $node.InnerText
}
```

Beispielausgabe:

```
Willkommen bei Example.com!
```

In diesen Beispielen ist `Invoke-WebRequest` am besten für einfache Aufgaben geeignet, während HtmlAgilityPack ein wesentlich umfangreicheres Set von Funktionen für komplexes HTML-Parsen und -Manipulation bietet.
