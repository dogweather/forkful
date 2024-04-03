---
date: 2024-01-20 17:42:37.267008-07:00
description: "Das L\xF6schen von Zeichen, die einem Muster entsprechen, bedeutet,\
  \ gezielt Buchstaben, Zahlen oder Symbole aus Strings zu entfernen, die bestimmten\u2026"
lastmod: '2024-03-13T22:44:54.085886-06:00'
model: gpt-4-1106-preview
summary: "Das L\xF6schen von Zeichen, die einem Muster entsprechen, bedeutet, gezielt\
  \ Buchstaben, Zahlen oder Symbole aus Strings zu entfernen, die bestimmten Kriterien\
  \ gen\xFCgen."
title: "L\xF6schen von Zeichen, die einem Muster entsprechen"
weight: 5
---

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
