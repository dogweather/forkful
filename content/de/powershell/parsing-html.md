---
title:                "Html-Analyse"
html_title:           "PowerShell: Html-Analyse"
simple_title:         "Html-Analyse"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/parsing-html.md"
---

{{< edit_this_page >}}

# Was & Warum?
HTML-Parsing ist das Extrahieren von Daten aus HTML-Quellcode. Programmierer nutzen dies, um Informationen von einer Webseite zu sammeln oder zu verarbeiten.

# Wie geht's:
```PowerShell
#installiere das Modul "HTML Agility Pack"
Install-Package HtmlAgilityPack -Source chocolatey

#lade die Webseite mit dem gewünschten Inhalt
$url = "https://www.example.com"

#parse die HTML-Daten
$webseite = Invoke-WebRequest -Uri $url 

#mit dem Modul "HTML Agility Pack" kannst du die Daten durchsuchen
$ergebnis = $webseite.ParsedHtml | Select-Xml -XPath "//div[@class='class-name']"

#das Ergebnis ist eine Collection mit allen HTML-Tags, die der XPath-Abfrage entsprechen
#z.B. um den Textinhalt der Tags zu erhalten:
$ergebnis | Select-Object -ExpandProperty "#text"
```

# Tiefere Einblicke:
HTML wird seit den Anfängen des Internets verwendet, um Webseiten zu erstellen. Mit dem Aufkommen von Web Scraping und Data Mining gewinnt das Parsing von HTML immer mehr an Bedeutung. Alternativ zu "HTML Agility Pack" gibt es auch andere Module wie "AngleSharp", die ähnliche Funktionen bieten. Das Modul "Invoke-WebRequest" ermöglicht es auch, mit anderen Datenformaten wie JSON oder XML zu arbeiten. Die Implementierung von HTML-Parsing kann komplex werden, wenn die HTML-Struktur nicht konsistent ist oder spezielle Tools wie Cookies oder Authentifizierung erforderlich sind.

# Siehe auch:
- [HTML Agility Pack Dokumentation] (https://html-agility-pack.net/) 
- ["Get-ParsedHtml: A PowerShell HTML CLI Built with the HTML Agility Pack" von rnelsonau] (https://www.powershellgallery.com/packages/Get-ParsedHtml) 
- ["AngleSharp" Modul Dokumentation] (https://anglesharp.github.io/)