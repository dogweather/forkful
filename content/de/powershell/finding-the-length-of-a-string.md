---
title:                "Die Länge eines Strings finden."
html_title:           "PowerShell: Die Länge eines Strings finden."
simple_title:         "Die Länge eines Strings finden."
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Ermitteln der Länge eines Strings ist eine wichtige Aufgabe für Programmierer. Es bedeutet einfach, die Anzahl der Zeichen in einem Text zu finden. Dies ist hilfreich für viele verschiedene Zwecke, wie zum Beispiel die Validierung von Benutzereingaben oder das Formatieren von Ausgaben.

## Wie geht's:

```powershell
# Beispiel 1:
$meinString = "Hallo, Welt!"
$meinString.Length
# Ausgabe: 12

# Beispiel 2:
$benutzereingabe = Read-Host "Bitte geben Sie Ihren Namen ein:"
$benutzereingabe.Length
# Eingabe: Max Mustermann
# Ausgabe: 13
```

## Tiefes Eintauchen:

Die Länge eines Strings zu finden ist keine neue Aufgabe. Es wurde schon seit den Anfängen des Programmierens verwendet. Früher mussten Programmierer möglicherweise eigene Funktionen erstellen, um die Länge eines Strings zu bestimmen. Heutzutage ist es jedoch in den meisten Sprachen eine integrierte Funktion. In PowerShell ist es die Eigenschaft "Length", die auf ein String-Objekt angewendet werden kann.

Es gibt auch alternative Methoden, um die Länge eines Strings zu ermitteln. Ein Beispiel ist die Verwendung von regulären Ausdrücken. Diese Methode erfordert jedoch etwas mehr Code und ist nicht so direkt wie die Verwendung der "Length"-Eigenschaft.

## Siehe auch:

- [Microsoft Dokumentation zur "Length"-Eigenschaft](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_strings?view=powershell-7.1#the-length-property)
- [Tutorial zu regulären Ausdrücken in PowerShell](https://www.click-net.ch/leinweber/extremadura/powershell_fundgrube/powershell_numericals/powershell_regulare_ausdrucke_tut.htm)