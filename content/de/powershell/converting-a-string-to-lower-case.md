---
title:                "Umwandeln eines Strings in Kleinbuchstaben"
html_title:           "PowerShell: Umwandeln eines Strings in Kleinbuchstaben"
simple_title:         "Umwandeln eines Strings in Kleinbuchstaben"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Was & Warum? 
Das Konvertieren einer Zeichenfolge in Kleinbuchstaben ist ein gängiger Programmierprozess, bei dem alle Buchstaben in einer Zeichenfolge in ihre entsprechenden Kleinbuchstaben umgewandelt werden. Programmierer verwenden dies in der Regel, um sicherzustellen, dass Zeichenfolgen korrekt sortiert oder verglichen werden können.

## How to:
Hier sind einige einfache Beispiele, wie Sie eine Zeichenfolge in PowerShell in Kleinbuchstaben konvertieren können:

```powershell
$string = "Hallo WELT!"
$string.ToLower()
# Ausgabe: hallo welt!
```

Sie können auch eine Zeichenfolge aus einer Variablen lesen und direkt in Kleinbuchstaben konvertieren:

```powershell
$string = Read-Host "Geben Sie eine Zeichenfolge ein"
$string.ToLower()
# Eingabe: Hallo WELT
# Ausgabe: hallo welt
```

## Deep Dive:
Historischer Kontext: Das Konzept der Konvertierung von Zeichen in Kleinbuchstaben reicht bis in die Anfänge der Programmierung zurück, als ASCII-Zeichen die Standardmethode zur Darstellung von Text waren. Mit der Einführung von Unicode im Jahr 1991 wurde die Konvertierung in Kleinbuchstaben komplexer und betraf nun mehrere Sprachen und Zeichensätze.

Alternativen: In PowerShell gibt es mehrere alternative Methoden, um eine Zeichenfolge in Kleinbuchstaben zu konvertieren. Sie können beispielsweise den Befehl `ToLower()` verwenden, der in diesem Artikel behandelt wurde, oder den Befehl `lc` verwenden.

Implementierungsdetails: PowerShell verwendet die Methode `ToLower()` aus der .NET-Klasse `String` zur Konvertierung von Zeichenfolgen in Kleinbuchstaben. Es ist wichtig zu beachten, dass diese Methode nicht nur englische Zeichenfolgen konvertiert, sondern auch Zeichenfolgen in anderen Sprachen.

# Siehe auch:
- [MSDN-Artikel zu ToLower-Methode] (https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower?view=netframework-4.8)
- [PowerShell-Dokumentation zu String-Klasse] (https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_string?view=powershell-6)