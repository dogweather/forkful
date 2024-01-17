---
title:                "Strings verketten"
html_title:           "PowerShell: Strings verketten"
simple_title:         "Strings verketten"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/concatenating-strings.md"
---

{{< edit_this_page >}}

# Was & Warum?
String-Konkatenation ist der Prozess der Verknüpfung von zwei oder mehr Zeichenketten zu einer längeren Zeichenkette. Programmierer verwenden dies, um effizient große Textblöcke aufzubauen oder Formatierungen vorzunehmen.

# Wie geht das?
```PowerShell
# Beispiel 1 - Einfache Konkatenation
$phrase1 = "Hallo"
$phrase2 = "Welt"
$newPhrase = $phrase1 + $phrase2
Write-Host $newPhrase
# Ausgabe: HalloWelt

# Beispiel 2 - Konkatenation mit Variablen
$person = "Tim"
$greeting = "Hallo $person,"
$message = "Wie geht es dir?"
$finalMessage = $greeting + $message
Write-Host $finalMessage
# Ausgabe: Hallo Tim, Wie geht es dir?
```

# Tief eintauchen
Historischer Kontext:
Die Konkatenation von Zeichenketten gehört zu den grundlegenden Funktionen einer Programmiersprache und wird seit den Anfängen der Programmierung verwendet.

Alternativen:
In PowerShell gibt es mehrere Möglichkeiten, Zeichenketten zu konkatenieren, wie z.B. die "-join"-Methode oder die Verwendung von String-Formatierung. Die Wahl der Methode hängt von den individuellen Bedürfnissen und Vorlieben des Entwicklers ab.

Implementierungsdetails:
In PowerShell werden Zeichenketten als Objekte behandelt. Bei der Konkatenation von Zeichenketten werden diese Objekte effizient zusammengefügt, anstatt sie zu kopieren und zusammenzufügen.

# Siehe auch
- Offizielle PowerShell-Dokumentation zu String-Konkatenation (https://docs.microsoft.com/de-de/powershell/module/microsoft.powershell.core/about/about_strings)
- Erweiterungsmethoden für String-Konkatenation in PowerShell (https://www.petri.com/string-concatenation-methods-powershell)