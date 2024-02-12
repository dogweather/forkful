---
title:                "Umformung eines Strings in Kleinbuchstaben"
aliases:
- /de/powershell/converting-a-string-to-lower-case/
date:                  2024-01-20T17:38:51.192520-07:00
model:                 gpt-4-1106-preview
simple_title:         "Umformung eines Strings in Kleinbuchstaben"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Was & Warum?
Strings in Kleinbuchstaben zu konvertieren bedeutet, alle Buchstaben eines Texts in untere Zeichen umzuwandeln. Programmierer nutzen diese Technik, um Texte einheitlich zu vergleichen und Formatunterschiede zu ignorieren.

## So geht's:
Ein Beispiel in PowerShell, um einen String kleinzuschreiben:

```PowerShell
$text = "PowerShell IST großartig!"
$lowercaseText = $text.ToLower()
Write-Output $lowercaseText
```

Ausgabe:

```
powershell ist großartig!
```

Ein weiteres Beispiel mit Pipe-Operator:

```PowerShell
"PowerShell IST auch SO!".ToLower() | Write-Output
```

Ausgabe:

```
powershell ist auch so!
```

## Tiefgang
Ursprünglich erforderte das Bearbeiten von Text in Skriptsprachen wie Bash oder PowerShell einen methodischen Ansatz, weil die Systeme zwischen Groß- und Kleinschreibung unterschieden. Die `ToLower()`-Methode in PowerShell nutzt .NET-Funktionen, um diese Wandlung einfach zu gestalten. Alternativen hierzu wären reguläre Ausdrücke oder ältere Command-Line-Tools, aber `ToLower()` ist deutlich direkter und performanter. Auch wird die kulturelle Variante des Texts berücksichtigt – Zeichen in Sprachen wie Deutsch, in denen "ß" zu "ss" wird, werden korrekt behandelt.

## Siehe auch:
- [.NET-Dokumentation zu String-Klasse und Methoden](https://docs.microsoft.com/dotnet/api/system.string?view=net-6.0)
