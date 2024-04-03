---
date: 2024-01-20 17:47:53.967215-07:00
description: "Die L\xE4nge eines Strings zu finden bedeutet, die Anzahl der Zeichen\
  \ zu z\xE4hlen, die er enth\xE4lt. Programmierer nutzen diese Information h\xE4\
  ufig, um Eingaben\u2026"
lastmod: '2024-03-13T22:44:54.092609-06:00'
model: gpt-4-1106-preview
summary: "Die L\xE4nge eines Strings zu finden bedeutet, die Anzahl der Zeichen zu\
  \ z\xE4hlen, die er enth\xE4lt."
title: "Ermittlung der Zeichenkettenl\xE4nge"
weight: 7
---

## How to:
PowerShell macht es einfach. Verwende die `.Length`-Eigenschaft eines Strings, um seine Länge zu erhalten:

```PowerShell
$beispielString = "Hallo, Welt!"
$laenge = $beispielString.Length
$laenge  # Gibt die Länge aus
```

Beispiel-Ausgabe:

```
12
```

Leere Strings und Whitespaces zählen auch:

```PowerShell
$leererString = ""
$leererString.Length  # Gibt 0 aus

$whitespaceString = " "
$whitespaceString.Length  # Gibt 1 aus
```

Beispiel-Ausgabe:

```
0
1
```

## Deep Dive
In früheren Programmiersprachen musstest du oft eine Schleife durchlaufen, um die Länge eines Strings zu bestimmen. PowerShell, als modernes Werkzeug, abstrahiert dies durch die `.Length`-Eigenschaft. 

Eine Alternative ist die `Length`-Property von .NET Objects, die in PowerShell genutzt werden kann, da PowerShell auf .NET basiert. Man könnte auch die `string.ToCharArray()`-Methode verwenden und dann die Länge des resultierenden Arrays bestimmen, aber das ist weniger direkt und performant.

Technisch gesehen verweist `.Length` auf ein Attribut des internen String-Objekts, welches die Anzahl der Zeichen speichert. Dies ist effizient, weil die Länge so nicht jedes Mal neu berechnet werden muss.

## See Also
Weitere Informationen und verwandte Themen:
- [PowerShell-Skripting-Guide](https://docs.microsoft.com/powershell/scripting/overview?view=powershell-7.1)
