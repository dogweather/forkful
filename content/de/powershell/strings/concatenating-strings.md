---
date: 2024-01-20 17:35:34.323893-07:00
description: "String-Konkatenation ist das Zusammenf\xFCgen von zwei oder mehreren\
  \ Textst\xFCcken (Strings) zu einem einzigen String. Programmierer nutzen dies,\
  \ um dynamische\u2026"
lastmod: '2024-03-13T22:44:54.093497-06:00'
model: gpt-4-1106-preview
summary: "String-Konkatenation ist das Zusammenf\xFCgen von zwei oder mehreren Textst\xFC\
  cken (Strings) zu einem einzigen String. Programmierer nutzen dies, um dynamische\u2026"
title: "Zeichenketten verkn\xFCpfen"
weight: 3
---

## Was & Warum?
String-Konkatenation ist das Zusammenfügen von zwei oder mehreren Textstücken (Strings) zu einem einzigen String. Programmierer nutzen dies, um dynamische Textausgaben zu erstellen, Werte in Textform zu integrieren oder um Strings für Dateipfade und URLs zu erzeugen.

## How to:
```PowerShell
# Variante 1: Plus-Operator (+)
$name = "Welt"
$greeting = "Hallo, " + $name + "!"
Write-Host $greeting  # Ausgabe: Hallo, Welt!

# Variante 2: Format-Operator (-f)
$greetingTemplate = "Hallo, {0}!"
$greeting = $greetingTemplate -f $name
Write-Host $greeting  # Ausgabe: Hallo, Welt!

# Variante 3: Join-Operator
$words = 'Hallo', 'Welt'
$greeting = [String]::Join(", ", $words) + "!"
Write-Host $greeting  # Ausgabe: Hallo, Welt!

# Variante 4: Interpolation mit Double Quotes (")
$greeting = "Hallo, $name!"
Write-Host $greeting  # Ausgabe: Hallo, Welt!
```

## Deep Dive
String-Konkatenation ist so alt wie die Programmierung selbst und es gibt viele Wege, dies in PowerShell zu tun, wie etwa mit dem Plus-Operator, dem Format-Operator, der Join-Methode oder String-Interpolation. Der Plus-Operator ist einfach und direkt, kann aber ineffizient sein bei der Konkatenation vieler Strings. Der Format-Operator bietet strukturierte String-Masken, ist aber weniger intuitiv. String-Interpolation ist ab PowerShell 5.0 verfügbar und ist lesbarer und effizienter für viele Anwendungsfälle. Alternative Methoden wie StringBuilder aus dem .NET Framework sind für Szenarien mit sehr vielen Konkatenationen nützlich, um Performance zu optimieren.

Bei der Arbeit mit Strings in PowerShell sollte auf den Speicherverbrauch und auf die Performance geachtet werden. Große und komplexe String-Operationen können das Programm verlangsamen. Für komplexe Szenarien ist das Arbeiten mit StringBuilder oder ähnlichen Konstrukten aus dem .NET Framework empfehlenswert.

## See Also
- MSDN Dokumentation über StringBuilder: [MSDN – StringBuilder Class](https://docs.microsoft.com/en-us/dotnet/api/system.text.stringbuilder?view=netframework-4.8)
