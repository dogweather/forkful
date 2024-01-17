---
title:                "Arbeiten mit JSON"
html_title:           "PowerShell: Arbeiten mit JSON"
simple_title:         "Arbeiten mit JSON"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/working-with-json.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Arbeiten mit JSON ist eine gängige Praxis in der Programmierung. Es bezieht sich auf den Austausch von Daten in einem bestimmten Format, das für Computer leicht lesbar ist. Programmierer verwenden JSON, um Informationen zwischen verschiedenen Anwendungen auszutauschen oder Daten zu speichern.

## Wie geht's?
Um mit JSON in PowerShell zu arbeiten, können wir das integrierte `ConvertTo-Json` und `ConvertFrom-Json` cmdlets verwenden. Hier sind ein paar Beispiele, wie wir damit umgehen können:

```PowerShell
# JSON-Objekt aus einer Datei erstellen
$jsonObject = Get-Content -Path .\meinedaten.json | ConvertFrom-Json

# JSON-Objekt aus einer Variable erstellen
$string = '{"Name": "Max", "Alter": 25}'
$jsonObject = $string | ConvertFrom-Json

# JSON-Objekt in eine Datei speichern
$jsonObject | ConvertTo-Json | Out-File -Path .\meinedaten.json

# JSON-Objekt durchsuchen und Eigenschaften abrufen
$jsonObject | Where-Object {$_.Name -eq "Max"} | Select-Object Alter
```

Die Ausgabe würde folgendermaßen aussehen:
```PowerShell 
Alter: 25
```

## Tiefsee-Tauchen
JSON wurde ursprünglich als Alternative zu XML entwickelt, um das Austauschen von Daten zwischen Anwendungen zu vereinfachen. Es ist leichtgewichtig, einfach zu verstehen und in verschiedenen Programmiersprachen unterstützt. Es gibt auch andere Methoden, um mit JSON in PowerShell zu arbeiten, wie z.B. der `ConvertFrom-Json` cmdlet. Außerdem können Entwickler JSON-Serialisierungsschemata verwenden, um strukturierte Daten zu speichern und abzurufen.

## Siehe auch
- Microsoft's Dokumentation zu [Working with JSON in PowerShell](https://docs.microsoft.com/en-us/powershell/scripting/learn/accepting-input/working-with-json-in-powershell?view=powershell-7.1) 
- JSON-Serialisierung mit [Newtonsoft.Json](https://www.newtonsoft.com/json)
- [Die offizielle JSON-Website](https://www.json.org/json-de.html)