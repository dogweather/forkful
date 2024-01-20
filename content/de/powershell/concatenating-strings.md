---
title:                "Strings verketten"
html_title:           "Bash: Strings verketten"
simple_title:         "Strings verketten"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/concatenating-strings.md"
---

{{< edit_this_page >}}

# Artikel: Verkettung von Zeichenketten in PowerShell

## Was & Warum?

Die Verkettung von Zeichenketten oder String Concatenation (auf Deutsch: Zeichenkettenzusammenführung) ist der Prozess der Verknüpfung von zwei oder mehr Zeichenketten in einer einzigen Instanz. Programmierer verwenden diese Technik, um Daten dynamisch zu erstellen, zu formatieren und zu manipulieren.

## Wie geht das?

In PowerShell gibt es viele Möglichkeiten, Zeichenketten zu verketten. Hier sind einige Beispiele:

**1. Verwendung des Pluszeichens (+):**

```PowerShell
$var1 = "Hallo"
$var2 = "Welt"
$var3 = $var1 + ", " + $var2
Write-Output $var3
```

Ausgabe:

```PowerShell
Hallo, Welt
```

**2. Verwendung der Formatmethode (-f):**

```PowerShell
$var1 = "Hallo"
$var2 = "Welt"
$var3 = "{0}, {1}" -f $var1, $var2
Write-Output $var3
```

Ausgabe:

```PowerShell
Hallo, Welt
```

## Deep Dive

Die Zeichenkettenverkettung hat eine lange Geschichte in der Programmierung, da sie häufig benötigt wird, um komplexe Datenstrukturen zu erstellen. 

Es gibt viele Alternativen zur Stringverkettung in PowerShell, einschließlich des Einsatzes von -join, der Format-Operator „-f“ und die Verwendung von Streams, um nur einige zu nennen. 

Bei der Implementierung ist zu beachten, dass beim Verketten von Zeichenketten in einer Schleife die Verwendung von Stringbuilder statt der direkten Verkettung empfohlen wird, um die Leistung zu verbessern. 

```PowerShell
$var = New-Object System.Text.StringBuilder
0..9 | ForEach-Object { $null = $var.Append($_) }
$finalVar = $var.ToString()
Write-Output $finalVar
```

Ausgabe:

```PowerShell
0123456789
```

## Siehe auch

Hier sind einige hilfreiche Ressourcen für zusätzliche Informationen und tieferes Lernen:

- Microsoft- Dokumentation zur PowerShell-Zeichenkettenmanipulation: [Link](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_operators?view=powershell-7.1#string-operators)
- Artikel auf StackOverflow über die Leistung von String-Verkettung in PowerShell: [Link](https://stackoverflow.com/questions/37519129/how-can-i-concatenate-strings-in-powershell)