---
title:                "Einsatz von regulären Ausdrücken"
date:                  2024-01-19
simple_title:         "Einsatz von regulären Ausdrücken"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Was & Warum?
Reguläre Ausdrücke, oft Regex genannt, sind Muster zur Textsuche und -manipulation. Programmierer verwenden sie, um Textdaten effizient zu durchsuchen, zu überprüfen und zu bearbeiten.

## How to:
PowerShell lässt dich leicht mit Regex arbeiten. Hier ein paar Beispiele:

```PowerShell
# Finde alle E-Mail-Adressen in einem Text
$text = "Kontaktiere uns unter info@example.com oder support@example.org."
$pattern = "\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z]{2,}\b"
[regex]::Matches($text, $pattern).Value

# Ergebnis:
# info@example.com
# support@example.org
```

```PowerShell
# Ersetze Zahlen durch das Wort 'Zahl'
$text = "Es gibt 3 Äpfel und 4 Birnen."
$pattern = "\d+"
$replacement = "Zahl"
$text -replace $pattern, $replacement

# Ergebnis:
# Es gibt Zahl Äpfel und Zahl Birnen.
```

```PowerShell
# Validiere ein deutsches Kfz-Kennzeichen
$licensePlate = "B-AB 1234"
$pattern = "^[A-Z]{1,3}-[A-Z]{1,2} \d{1,4}$"
if ($licensePlate -match $pattern) {
    "$licensePlate ist ein gültiges Kennzeichen."
} else {
    "$licensePlate ist kein gültiges Kennzeichen."
}

# Ergebnis:
# B-AB 1234 ist ein gültiges Kennzeichen.
```

## Deep Dive
Die Nutzung von Regex geht auf die 1950er Jahre zurück und ist tief in der Informatik verwurzelt. Alternativen zu Regex sind spezialisierte Parser, Text-to-SQL-Abfragekonverter und String-Funktionen, die ohne Mustererkennung auskommen, aber weniger mächtig sind. PowerShell verwendet das .NET Regex-Objekt, welches eine Implementation der Regex-Funktionalität mit Zusatzfeatures wie benannten Gruppen und Lookaheads bietet.

## See Also
- Online Regex Tester und Debugger: [regex101](https://regex101.com/)
- Einsteigerfreundliches Regex-Tutorial: [RegexOne](https://regexone.com/)
