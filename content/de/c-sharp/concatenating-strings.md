---
title:                "Zeichenketten verknüpfen"
date:                  2024-01-20T17:34:31.700287-07:00
model:                 gpt-4-1106-preview
simple_title:         "Zeichenketten verknüpfen"

category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Zusammenfügen von Zeichenketten, auch "String Concatenation" genannt, ist ziemlich grundlegend: Es geht darum, zwei oder mehrere Strings zu einem einzigen zu kombinieren. Wir machen dies ständig, um dynamische Texte zu erzeugen, Pfade zu bauen, Mitteilungen für Benutzer zu generieren und so weiter.

## So geht's:
Hier sind ein paar einfache Beispiele, wie man in C# Strings zusammenfügt:

```csharp
string gruss = "Hallo";
string ziel = "Welt";
string satz = gruss + " " + ziel + "!";
Console.WriteLine(satz); // Ausgabe: Hallo Welt!

// Mit String Interpolation
string satz2 = $"{gruss} {ziel}!";
Console.WriteLine(satz2); // Ausgabe: Hallo Welt!

// Mit String.Concat
string satz3 = String.Concat(gruss, " ", ziel, "!");
Console.WriteLine(satz3); // Ausgabe: Hallo Welt!

// Mit StringBuilder
var builder = new StringBuilder();
builder.Append(gruss);
builder.Append(" ");
builder.Append(ziel);
builder.Append("!");
Console.WriteLine(builder.ToString()); // Ausgabe: Hallo Welt!
```

## Deep Dive
In den frühen Tagen von .NET war die String-Konkatenation mit dem `+`-Operator gängig. Das erschien natürlich, aber es kann bei häufigem Gebrauch ineffizient sein, weil Strings unveränderbar (immutable) sind. Jedes Mal, wenn du zwei Strings zusammenfügst, erzeugst du tatsächlich ein ganz neues String-Objekt.

Alternativen wie `StringBuilder` entstanden, um dieses Problem zu lösen, vor allem in Szenarien, wo man viele Concatenationen durchführt. `StringBuilder` arbeitet effizient, da es nicht bei jeder Anhängung ein neues String-Objekt erstellt.

String Interpolation (eingeführt in C# 6) und `String.Concat` sind weitere Alternativen. Interpolation ist weniger fehleranfällig und lesefreundlicher, besonders bei komplexen Zusammensetzungen. `String.Concat` kann gut sein für das Zusammenfügen einer Vielzahl von Strings, weil es direkt auf das finale Stringlängenziel hinarbeitet.

Es ist wichtig zu wissen, wann du welche Methode nutzen solltest. `StringBuilder` ist König, wenn es um massenhafte oder komplexe Konkatenationen geht. Für einfache, einmalige Zusammensetzungen tut es der `+`-Operator oder String Interpolation auch.

## Siehe auch
- Die [offizielle Dokumentation](https://docs.microsoft.com/de-de/dotnet/csharp/programming-guide/strings/) bietet weitere Details und Beispiele zur Arbeit mit Strings in C#.
- Microsofts [Performance Tips](https://docs.microsoft.com/de-de/dotnet/csharp/write-safe-efficient-code#string-and-text-handling) zum Thema String-Manipulation und wie man effizienten Code schreibt.
- [String vs StringBuilder](https://docs.microsoft.com/de-de/dotnet/standard/base-types/stringbuilder) vergleicht Performance und Use Cases.
