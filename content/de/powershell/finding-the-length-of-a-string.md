---
title:                "Die Länge eines Strings ermitteln"
html_title:           "Java: Die Länge eines Strings ermitteln"
simple_title:         "Die Länge eines Strings ermitteln"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Die Länge eines Strings zu finden (also die Anzahl der Zeichen), ist ein häufiger Aspekt der Programmierung. Sie steuert, von der Benutzereingabenverifizierung bis hin zur Datenmanipulation, viele Szenarien in praktisch allen Anwendungsbereichen.

## So geht's:

In PowerShell bestimmen wir die Länge eines Strings mit dem `.Length` Property:

```PowerShell
$stringVariable = "Hallo Welt"
$stringVariable.Length
```

Die Ausgabe wird `11` sein, weil "Hallo Welt" 11 Zeichen hat (inklusive Leerzeichen).

## Deep Dive

Historisch gesehen hat das Auffinden von String-Längen seine Wurzeln in den frühen Tagen der Computerprogrammierung und ist in praktisch allen Programmiersprachen vorhanden. PowerShell nutzt ähnlich wie C# und Java das `.Length` Property, was an die gemeinsame Herkunft aus der C-Familie erinnert.

Als Alternative zu `.Length` könnte man eine Funktion schreiben, die jeden Buchstaben im String durchläuft. Aber `.Length` ist effizienter und betont den objektorientierten Ansatz von PowerShell.

Das `.Length` Property wird tatsächlich von der System.String-Klasse in .NET zur Verfügung gestellt, auf die PowerShell zugreift. Daher ist dessen Verwendung extrem performant.

## Siehe auch

1. [Microsoft Docs: String.Length Property](https://docs.microsoft.com/de-de/dotnet/api/system.string.length?view=net-5.0)
2. [About Strings in PowerShell](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_strings?view=powershell-7.1)
3. [PowerShell String Manipulation](https://adamtheautomator.com/powershell-string-manipulation/)