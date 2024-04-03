---
date: 2024-01-20 17:38:51.192520-07:00
description: 'So geht''s: Ein Beispiel in PowerShell, um einen String kleinzuschreiben.'
lastmod: '2024-03-13T22:44:54.088824-06:00'
model: gpt-4-1106-preview
summary: Ein Beispiel in PowerShell, um einen String kleinzuschreiben.
title: Umformung eines Strings in Kleinbuchstaben
weight: 4
---

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
