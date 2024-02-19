---
aliases:
- /de/powershell/parsing-html/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:35.163566-07:00
description: "Das Parsen von HTML in PowerShell bedeutet, HTML-Inhalte zu untersuchen,\
  \ um spezifische Daten zu extrahieren oder webbezogene Aufgaben zu automatisieren.\u2026"
lastmod: 2024-02-18 23:09:05.098910
model: gpt-4-0125-preview
summary: "Das Parsen von HTML in PowerShell bedeutet, HTML-Inhalte zu untersuchen,\
  \ um spezifische Daten zu extrahieren oder webbezogene Aufgaben zu automatisieren.\u2026"
title: HTML parsen
---

{{< edit_this_page >}}

## Was & Warum?
Das Parsen von HTML in PowerShell bedeutet, HTML-Inhalte zu untersuchen, um spezifische Daten zu extrahieren oder webbezogene Aufgaben zu automatisieren. Programmierer tun dies, um mit Webseiten zu interagieren, Webinhalte zu scrapen oder Formulareinreichungen und andere Webinteraktionen ohne die Notwendigkeit eines Webbrowsers zu automatisieren.

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
