---
date: 2024-01-26 04:44:08.341768-07:00
description: "Wie zu: PowerShell hat keine eingebaute Unterst\xFCtzung f\xFCr komplexe\
  \ Zahlen, daher m\xFCssen Sie entweder Ihre eigene L\xF6sung entwickeln oder .NET's\u2026"
lastmod: '2024-03-13T22:44:54.095630-06:00'
model: gpt-4-0125-preview
summary: "PowerShell hat keine eingebaute Unterst\xFCtzung f\xFCr komplexe Zahlen,\
  \ daher m\xFCssen Sie entweder Ihre eigene L\xF6sung entwickeln oder .NET's `System.Numerics.Complex`\
  \ verwenden."
title: Umgang mit komplexen Zahlen
weight: 14
---

## Wie zu:
PowerShell hat keine eingebaute Unterstützung für komplexe Zahlen, daher müssen Sie entweder Ihre eigene Lösung entwickeln oder .NET's `System.Numerics.Complex` verwenden.

```PowerShell
# Lassen Sie uns komplexe Zahlen mit .NET machen
[Reflection.Assembly]::LoadWithPartialName("System.Numerics") | Out-Null

# Komplexe Zahlen erstellen
$complex1 = [System.Numerics.Complex]::new(3, 4) # 3 + 4i
$complex2 = [System.Numerics.Complex]::new(1, 2) # 1 + 2i

# Zwei komplexe Zahlen addieren
$sum = [System.Numerics.Complex]::Add($complex1, $complex2) # 4 + 6i

# Zwei komplexe Zahlen multiplizieren
$product = [System.Numerics.Complex]::Multiply($complex1, $complex2) # -5 + 10i

# Die Ergebnisse anzeigen
"Summe: $sum"
"Produkt: $product"
```
Ausgabe:
```
Summe: (4, 6)
Produkt: (-5, 10)
```

## Vertiefung
Komplexe Zahlen wurden im 16. Jahrhundert entwickelt, um Gleichungen zu lösen, die keine Lösungen im Bereich der reellen Zahlen hatten. Sie sind jetzt ein Eckpfeiler der modernen Mathematik.

Die Abhängigkeit von PowerShell von .NET für die Unterstützung komplexer Zahlen bedeutet, dass die Leistung solide ist. Alternativen umfassen Drittanbieter-Bibliotheken oder andere Programmiersprachen wie Python, wo komplexe Zahlen ein nativer Datentyp sind.

## Siehe auch
- [System.Numerics.Complex Struktur](https://docs.microsoft.com/de-de/dotnet/api/system.numerics.complex)
- [Komplexe Zahl Arithmetik in Python](https://docs.python.org/3/library/cmath.html)
