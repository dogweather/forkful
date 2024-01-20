---
title:                "Verwendung regulärer Ausdrücke"
html_title:           "PowerShell: Verwendung regulärer Ausdrücke"
simple_title:         "Verwendung regulärer Ausdrücke"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Was & Warum?
Reguläre Ausdrücke sind eine Möglichkeit, in Programmen Text zu durchsuchen und zu manipulieren. Programmierer verwenden sie, um effizient und genau bestimmte Teile von Strings zu finden oder anzupassen.

## So geht's:
```PowerShell
# Beispiel 1: Suchen eines bestimmten Wortes
$text = "Dies ist ein Beispieltext."
$text -match "Beispiel"
# Output:
True

# Beispiel 2: Ersetzen eines bestimmten Musters
$text = "Die Telefonnummer ist 123-456-7890."
$text -replace "\d{3}-\d{3}-\d{4}", "555-555-5555"
# Output:
Die Telefonnummer ist 555-555-5555.
```

## Eintauchen:
Reguläre Ausdrücke haben ihren Ursprung in der Theoretischen Informatik und wurden erstmals in den 1950er Jahren von Stephen Cole Kleene eingeführt. Eine Alternative zu regulären Ausdrücken ist die Verwendung von String-Methoden in verschiedenen Programmiersprachen. In PowerShell können reguläre Ausdrücke mit dem -match und -replace Operator verwendet werden.

## Siehe auch:
- [PowerShell Regular Expressions Documentation](https://docs.microsoft.com/powershell/module/microsoft.powershell.core/about/about_regular_expressions?view=powershell-7.1)