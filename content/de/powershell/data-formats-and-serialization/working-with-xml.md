---
date: 2024-01-26 04:34:14.586588-07:00
description: "Die Arbeit mit XML umfasst die Manipulation und den Zugriff auf Daten,\
  \ die in der eXtensible Markup Language strukturiert sind. Programmierer arbeiten\
  \ mit\u2026"
lastmod: '2024-03-13T22:44:54.127803-06:00'
model: gpt-4-0125-preview
summary: "Die Arbeit mit XML umfasst die Manipulation und den Zugriff auf Daten, die\
  \ in der eXtensible Markup Language strukturiert sind. Programmierer arbeiten mit\u2026"
title: Arbeiten mit XML
---

{{< edit_this_page >}}

## Was & Warum?
Die Arbeit mit XML umfasst die Manipulation und den Zugriff auf Daten, die in der eXtensible Markup Language strukturiert sind. Programmierer arbeiten mit XML, um die Interoperabilität mit anderen Systemen zu ermöglichen oder Konfigurationsdateien, Datenfeeds und andere strukturierte Dokumente, die in Webdiensten üblich sind, zu lesen und zu schreiben.

## Wie geht das:
```PowerShell
# Laden einer XML-Datei in eine Variable
[xml]$xmlInhalt = Get-Content 'Pfad\zu\Ihrer\Datei.xml'

# Zugreifen auf XML-Knoten
$buecher = $xmlInhalt.katalog.buch
foreach ($buch in $buecher) {
  Write-Output "Titel: $($buch.titel)"
}

# Erstellen eines neuen XML-Elements
$neuesBuch = $xmlInhalt.CreateElement("buch")
$neuesBuch.SetAttribute("id", "bk999")
$xmlInhalt.DocumentElement.AppendChild($neuesBuch)

# Die XML wieder in eine Datei speichern
$xmlInhalt.Save('Pfad\zu\Ihrer\aktualisierten\Datei.xml')
```
Beispielausgabe:
```
Titel: Programmieren mit PowerShell
Titel: XML Essentials
```

## Tiefer eintauchen
XML, oder eXtensible Markup Language, gibt es seit den späten 90ern und bleibt ein weit verbreitetes Format für strukturierte Daten. PowerShell vereinfacht die Arbeit mit XML im Vergleich zu traditionellen Parsing-Methoden; es wandelt XML direkt in Objekte um, sodass Sie mit Elementen durch die vertraute Punkt-Notation interagieren können.

Alternativen zu XML sind JSON, YAML oder benutzerdefinierte Datenformate. JSON hat beispielsweise aufgrund seiner Leichtgewichtigkeit und Benutzerfreundlichkeit mit Webtechnologien an Beliebtheit gewonnen. Allerdings machen XMLs erweiterte Funktionen wie Namensräume, Schemata und XSLT-Verarbeitung es oft besser geeignet für komplexe Dokumente oder Industriestandards.

PowerShell nutzt die XML-Fähigkeiten des .NET Frameworks für seine XML-Behandlung. Das bedeutet, es geht nicht nur um einfache Lese-Schreib-Operationen; Sie können auch mit XML-Schemata für die Validierung arbeiten, XPath für Abfragen verwenden und XSLT-Transformationen durchführen, alles über PowerShell.

## Siehe auch
- [W3Schools XML-Tutorial](https://www.w3schools.com/xml/)
- [XML vs. JSON](https://www.json.org/json-de.html)
