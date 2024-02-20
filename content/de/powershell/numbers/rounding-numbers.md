---
date: 2024-01-26 03:46:07.469761-07:00
description: "Das Runden von Zahlen bedeutet, einen Wert auf die n\xE4chste ganze\
  \ Zahl oder die angegebene Dezimalstelle zu justieren. Programmierer runden Zahlen,\
  \ um\u2026"
lastmod: 2024-02-19 22:05:13.028354
model: gpt-4-0125-preview
summary: "Das Runden von Zahlen bedeutet, einen Wert auf die n\xE4chste ganze Zahl\
  \ oder die angegebene Dezimalstelle zu justieren. Programmierer runden Zahlen, um\u2026"
title: Zahlen runden
---

{{< edit_this_page >}}

## Was & Warum?
Das Runden von Zahlen bedeutet, einen Wert auf die nächste ganze Zahl oder die angegebene Dezimalstelle zu justieren. Programmierer runden Zahlen, um Daten zu vereinfachen, die Lesbarkeit zu verbessern oder bestimmten mathematischen Anforderungen während der Berechnungen gerecht zu werden.

## Wie:
Sie haben einige praktische Cmdlets und Methoden in PowerShell zum Runden:

- `Round()`-Methode aus der Math-Klasse
```PowerShell
[Math]::Round(15.68) # Rundet auf 16
```
- Dezimalstellen angeben:
```PowerShell
[Math]::Round(15.684, 2) # Rundet auf 15.68
```
- `Ceiling()` und `Floor()`, für das stets Aufrunden oder Abrunden:
```PowerShell
[Math]::Ceiling(15.2) # Rundet auf 16 auf
[Math]::Floor(15.9) # Rundet auf 15 ab
```

## Tiefergehende Betrachtung
Das Runden von Zahlen ist kein Neuling; es gibt es seit der Antike, nützlich für Handel, Wissenschaft und Zeitmessung. Bezüglich PowerShell folgt `[Math]::Round()` standardmäßig dem "Banker's Rounding", bei dem 0,5 auf die nächstgelegene gerade Zahl geht, um Verzerrungen in statistischen Operationen zu reduzieren.

Sie sind allerdings nicht nur auf `[Math]` Methoden beschränkt. Mehr Kontrolle gefragt? Schauen Sie sich `[System.Math]::Round(Number, Digits, MidpointRounding)` an, wo Sie festlegen können, wie Mittelpunkte behandelt werden: weg von Null oder auf gerade (auch bekannt als Banker-Rounding).

Eine andere Perspektive: das `System.Globalization.CultureInfo`-Objekt. Es hilft bei der berücksichtigung von lokalen Formatierungs- und Rundungsvorlieben, wenn es um internationale Zahlen geht.

## Siehe auch
- Microsofts offizielle Dokumentation zu Math-Methoden: [Link](https://learn.microsoft.com/de-de/dotnet/api/system.math?view=net-7.0)
- Spezifika des Dezimalrundens in .NET: [Link](https://learn.microsoft.com/de-de/dotnet/api/system.midpointrounding?view=net-7.0)
- Diskussionen über Rundungen auf StackOverflow: [Link](https://stackoverflow.com/questions/tagged/rounding+powershell)
