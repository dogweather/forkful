---
title:                "HTML parsen"
html_title:           "Arduino: HTML parsen"
simple_title:         "HTML parsen"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/parsing-html.md"
---

{{< edit_this_page >}}

# PowerShell HTML Parsing für Programmierer: Ein Quick-Guide

In diesem Artikel geht es um einen kurzen Überblick über HTML Parsing mit PowerShell. Lassen Sie uns direkt einsteigen.

## Was & Warum?

HTML-Parsing bezieht sich auf das Durchsuchen und Extrahieren relevanter Daten in einem HTML-String. Programmierer machen dieses meist, um Informationen aus Webseiten zu gewinnen, die in einer für die Anwendung nützlichen Weise dargestellt werden können.

## So geht's:

Mit PowerShell ist HTML-Parsing einfach. Hier ist ein einfacher Weg, es mit der Invoke-WebRequest-Funktion zu tun:

```PowerShell
# Webseite aufrufen
$webseite = Invoke-WebRequest -Uri "https://IhreWebseite.de"

# Parsing 
$parsedHtml = $webseite.ParsedHtml.body.innerText

# Ausgabe
$parsedHtml
```
Es gibt noch einen weiteren Weg, mit Hilfe des HtmlAgilityPack:

```PowerShell
# HtmlAgilityPack laden
Add-Type -Path "HtmlAgilityPack.dll"

# Neue HtmlDocument Instanz erstellen
$html = New-Object HtmlAgilityPack.HtmlDocument

# HtmlDocument laden mit HTML-Code
$html.LoadHtml($webseite.Content)

# Daten extrahieren
$daten = $html.DocumentNode.SelectNodes('//tag')

# Daten ausgeben
$daten.innerText
```

## Deep Dive

Der Ursprung des HTML-Parsings liegt in den Wurzeln des Webs. Als das Web gebaut wurde, wurde der Code lesefreundlich gemacht, damit die Menschen die Informationen lesen können. Im Laufe der Zeit entwickelten sich Methoden, um Daten aus HTML zu extrahieren.

In PowerShell bekam man die Möglichkeit, HTML zu parsen, als Invoke-WebRequest in PowerShell 3.0 eingeführt wurde. Es gibt Alternativen zum Parsing von HTML mit PowerShell, einschließlich der Verwendung von .NET Framework Bibliotheken wie HtmlAgilityPack.

Es besteht sogar die Möglichkeit, reguläre Ausdrücke zu verwenden, aber im Kontext von HTML ist das in den meisten Fällen keine empfehlenswerte Methode.

## Weiterführendes Material

Hier sind einige Ressourcen, die Ihnen helfen können, Ihre Kenntnisse über PowerShell und HTML Parsing zu vertiefen:

- [PowerShell-Dokumentation](https://docs.microsoft.com/de-de/powershell/)
- [Invoke-WebRequest](https://docs.microsoft.com/de-de/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-7.1)
- [HtmlAgilityPack](https://html-agility-pack.net/)
- [Einführung in HTML-Parsing](https://realpython.com/python-web-scraping-practical-introduction/) (Englisch)
- [Vergleich von Scrape und Parse](https://www.octoparse.de/blog/der-unterschied-zwischen-web-scraping-und-html-parsing)