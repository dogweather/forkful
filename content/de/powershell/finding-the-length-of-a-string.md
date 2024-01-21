---
title:                "Ermittlung der Zeichenkettenlänge"
date:                  2024-01-20T17:47:53.967215-07:00
model:                 gpt-4-1106-preview
simple_title:         "Ermittlung der Zeichenkettenlänge"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Die Länge eines Strings zu finden bedeutet, die Anzahl der Zeichen zu zählen, die er enthält. Programmierer nutzen diese Information häufig, um Eingaben zu validieren, Daten zu verarbeiten oder bei der Textmanipulation.

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