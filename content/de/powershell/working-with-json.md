---
title:                "Arbeiten mit JSON"
date:                  2024-01-19
html_title:           "Arduino: Arbeiten mit JSON"
simple_title:         "Arbeiten mit JSON"

category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/working-with-json.md"
---

{{< edit_this_page >}}

## Was & Warum?
JSON (JavaScript Object Notation) ist ein leichtes Austauschformat, das menschenlesbar und für Maschinen einfach zu parsen ist. Programmierer nutzen JSON, weil es Webstandards unterstützt und das Teilen von Daten zwischen verschiedenen Programmiersprachen vereinfacht.

## How to:
```PowerShell
# JSON-String in PS-Objekt umwandeln
$jsonString = '{"name": "Max", "age": 28}'
$person = $jsonString | ConvertFrom-Json
$person.name  # Ausgabe: Max

# PS-Objekt in JSON-String umwandeln
$personObj = [PSCustomObject]@{
    name = 'Lisa'
    age = 25
}
$json = $personObj | ConvertTo-Json
$json  # Ausgabe: {"name":"Lisa","age":25}
```

## Deep Dive
JSON wurde Anfang der 2000er Jahre populär, als Webdienste und AJAX Anrufe mehr verbreitet waren. Alternativen zu JSON sind XML oder YAML, aber JSON ist wegen seiner Einfachheit und Geschwindigkeit bevorzugt. Beim Umgang mit JSON in PowerShell verwendet ConvertFrom-Json .NET-Methoden zum Parsen des Strings und ConvertTo-Json zum Konvertieren des Objekts zurück in einen String.

## Siehe Auch
- Offizielle PowerShell-Dokumentation zu JSON: [link]
- JSON-Standard-Spezifikation von ECMA: [link]
- Vergleich von JSON-Alternativen (XML, YAML): [link]
