---
date: 2024-01-20 17:37:51.458605-07:00
description: "So geht's: Das `ToLowerInvariant()` ber\xFCcksichtigt keine lokale kulturelle\
  \ Unterschiede. Es gibt aber auch `ToLower()`, das die Kultur des aktuellen\u2026"
lastmod: '2024-04-05T21:53:55.759394-06:00'
model: gpt-4-1106-preview
summary: "Das `ToLowerInvariant()` ber\xFCcksichtigt keine lokale kulturelle Unterschiede."
title: Umformung eines Strings in Kleinbuchstaben
weight: 4
---

## So geht's:
```C#
string original = "Hallo Welt!";
string klein = original.ToLowerInvariant();

Console.WriteLine(klein);  // Ausgabe: hallo welt!
```
Das `ToLowerInvariant()` berücksichtigt keine lokale kulturelle Unterschiede. Es gibt aber auch `ToLower()`, das die Kultur des aktuellen Threads berücksichtigt. Hier ein Beispiel:

```C#
string original = "Straße";
string kleinInvariant = original.ToLowerInvariant();
string klein = original.ToLower();

Console.WriteLine(kleinInvariant); // Ausgabe: strasse
Console.WriteLine(klein); // Ausgabe: straße (in deutscher Kultur)
```

## Tiefgang:
Früher, als die internationale Softwareentwicklung nicht so verbreitet war, gab es meist nur `ToLower()`, das abhängig von der Kultur des Betriebssystems war. Heute ist die invariante Methode wichtig, weil Software global genutzt wird.

Alternativen: Neben `.ToLower()` und `.ToLowerInvariant()` kann man mit `String.Compare()` und `String.Equals()` mit dem Parameter `StringComparison.OrdinalIgnoreCase` vergleichen, ohne den String zu verändern.

Implementierungsdetails: `.ToLowerInvariant()` und `.ToLower()` benutzen beide die Unicode-Regeln für die Kleinbuchstaben-Umwandlung. Sie durchlaufen den String und ersetzen jeden Großbuchstaben durch seinen kleinbuchstabigen Gegenpart.

## Siehe auch:
- [Microsoft Dokumentation zu ToLowerInvariant](https://docs.microsoft.com/dotnet/api/system.string.tolowerinvariant)
- [Microsoft Dokumentation zu ToLower](https://docs.microsoft.com/dotnet/api/system.string.tolower)
- [Unicode Standard zur Groß- und Kleinschreibung](https://unicode.org/reports/tr21/tr21-5.html)
