---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:38.762009-07:00
description: "Die Arbeit mit JSON in der Fish Shell umfasst das Parsen und Generieren\
  \ von JSON-Daten, eine g\xE4ngige Aufgabe f\xFCr die Konfiguration von Anwendungen,\
  \ API-\u2026"
lastmod: '2024-03-13T22:44:54.330074-06:00'
model: gpt-4-0125-preview
summary: "Die Arbeit mit JSON in der Fish Shell umfasst das Parsen und Generieren\
  \ von JSON-Daten, eine g\xE4ngige Aufgabe f\xFCr die Konfiguration von Anwendungen,\
  \ API-\u2026"
title: Arbeiten mit JSON
---

{{< edit_this_page >}}

## Was & Warum?

Die Arbeit mit JSON in der Fish Shell umfasst das Parsen und Generieren von JSON-Daten, eine gängige Aufgabe für die Konfiguration von Anwendungen, API-Interaktionen und die Optimierung von Befehlszeilen-Workflows. Angesichts der Allgegenwart von JSON in der Web- und Anwendungsentwicklung kann die Beherrschung seiner Manipulation direkt in der Shell die Automatisierung und Effizienz der Datenverarbeitung für Programmierer erheblich verbessern.

## Wie geht das:

Die Fish Shell selbst verfügt nicht über integrierte Hilfsprogramme zum Parsen und Generieren von JSON. Sie integriert sich jedoch nahtlos in Drittanbieter-Tools wie `jq` für die JSON-Verarbeitung. `jq` ist ein leistungsstarker und vielseitiger Befehlszeilen-JSON-Prozessor, der es Ihnen ermöglicht, strukturierte Daten einfach und ausdrucksstark zu schneiden, zu filtern, zu mappen und zu transformieren.

### JSON mit jq parsen
Um eine JSON-Datei zu parsen und Daten mit `jq` zu extrahieren:

```fish
# Angenommen, Sie haben eine JSON-Datei namens 'data.json' mit dem Inhalt: {"name":"Fish Shell","version":"3.4.0"}
cat data.json | jq '.name'
# Beispiel Ausgabe
"Fish Shell"
```

### JSON mit jq generieren
JSON-Inhalte aus Shell-Variablen oder -Ausgaben erstellen:

```fish
# JSON-Objekt aus Variablen erstellen
set name "Fish Shell"
set version "3.4.0"
jq -n --arg name "$name" --arg version "$version" '{name: $name, version: $version}'
# Beispiel Ausgabe
{
  "name": "Fish Shell",
  "version": "3.4.0"
}
```

### JSON-Sammlungen filtern
Angenommen, wir haben ein JSON-Array von Objekten in einer Datei namens `versions.json`:
```json
[
  {"version": "3.1.2", "stable": true},
  {"version": "3.2.0", "stable": false},
  {"version": "3.4.0", "stable": true}
]
```
Um dieses Array nur nach stabilen Versionen zu filtern:

```fish
cat versions.json | jq '.[] | select(.stable == true) | .version'
# Beispiel Ausgabe
"3.1.2"
"3.4.0"
```

Die bereitgestellten Beispiele demonstrieren die Kraft der Integration von `jq` in die Fish Shell für JSON-Operationen. Die Nutzung solcher Tools bereichert das Erlebnis in der Shell und macht sie zu einer formidablen Umgebung für die Handhabung moderner Datenformate.
