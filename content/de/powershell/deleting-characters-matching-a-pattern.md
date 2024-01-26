---
title:                "Löschen von Zeichen, die einem Muster entsprechen"
date:                  2024-01-20T17:42:37.267008-07:00
model:                 gpt-4-1106-preview
simple_title:         "Löschen von Zeichen, die einem Muster entsprechen"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Löschen von Zeichen, die einem Muster entsprechen, bedeutet, gezielt Buchstaben, Zahlen oder Symbole aus Strings zu entfernen, die bestimmten Kriterien genügen. Programmierer verwenden diesen Prozess, um Daten zu bereinigen, Eingaben zu validieren oder Formate zu standardisieren.

## How to:
Lassen wir die Worte und kommen direkt zum Code.

```PowerShell
$text = "Hallo Welt 123!"
$pattern = '[0-9]'
$cleanText = $text -replace $pattern, ''
Write-Host $cleanText
```
Ausgabe:
```
Hallo Welt !
```

Hier wird durch `-replace` alles entfernt, was auf das Muster `[0-9]`, also alle Ziffern, passt.

## Deep Dive
Das Löschen von Zeichen, die einem Muster entsprechen, nutzt die Kraft regulärer Ausdrücke (regular expressions, regex), die seit den 1950er Jahren existieren. In PowerShell wird dies häufig mit dem `-replace`-Operator erreicht. Alternativ kann man `Remove` oder `Trim` verwenden, wenn die Anforderungen einfacher sind, das bedeutet jedoch, auf die Flexibilität von Regex zu verzichten. Die Implementierung von Regex in PowerShell ist effizient und nutzt die .NET System.Text.RegularExpressions-Namespace, was bedeutet, dass alle Methoden und Eigenschaften, die in .NET verfügbar sind, auch in PowerShell verfügbar sind.

## See Also
- Microsoft-Dokumentation zu regulären Ausdrücken in .NET: [Regular Expression Language - Quick Reference](https://docs.microsoft.com/dotnet/standard/base-types/regular-expression-language-quick-reference)
