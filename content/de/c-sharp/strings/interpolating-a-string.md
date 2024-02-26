---
changelog:
- 2024-02-25, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-02-25 17:06:56.306880-07:00
description: "Die Zeichenketteninterpolation in C# erm\xF6glicht es Ihnen, einen neuen\
  \ String zu erstellen, indem Ausdr\xFCcke innerhalb eines Zeichenketten-Literals\u2026"
lastmod: '2024-02-25T18:49:50.935152-07:00'
model: gpt-4-0125-preview
summary: "Die Zeichenketteninterpolation in C# erm\xF6glicht es Ihnen, einen neuen\
  \ String zu erstellen, indem Ausdr\xFCcke innerhalb eines Zeichenketten-Literals\u2026"
title: Interpolation eines Strings
---

{{< edit_this_page >}}

## Was & Warum?
Die Zeichenketteninterpolation in C# ermöglicht es Ihnen, einen neuen String zu erstellen, indem Ausdrücke innerhalb eines Zeichenketten-Literals eingefügt werden. Dies erleichtert das Formatieren und Verketten von Strings. Programmierer nutzen dieses Feature, um die Lesbarkeit und Wartbarkeit des Codes zu verbessern, insbesondere wenn sie mit dynamischen Zeichenketteninhalten arbeiten.

## Wie geht das:
In C# wird die Zeichenketteninterpolation durch ein Dollarzeichen (`$`) angezeigt, gefolgt von einem Zeichenketten-Literal. Die Variablennamen oder Ausdrücke werden in geschweifte Klammern (`{}`) eingeschlossen.

```csharp
string name = "Jane";
int age = 28;
string interpolatedString = $"Hallo, {name}! Du bist {age} Jahre alt.";
Console.WriteLine(interpolatedString);
// Ausgabe: Hallo, Jane! Du bist 28 Jahre alt.
```

In einem komplexeren Beispiel können Sie Operationen ausführen oder Methoden innerhalb der geschweiften Klammern aufrufen:

```csharp
double price = 19.99;
int quantity = 3;
string orderDetail = $"Gesamtpreis: {price * quantity:C2}";
Console.WriteLine(orderDetail);
// Ausgabe: Gesamtpreis: $59.97
```
Der `:C2` Formatierungshinweis innerhalb der geschweiften Klammern formatiert die Zahl als Währung mit zwei Dezimalstellen.

Für Szenarien, die eine ausgefeiltere Formatierung oder Lokalisierung erfordern, könnten Sie die Methode `string.Format` in Betracht ziehen oder Bibliotheken wie Humanizer verwenden. Humanizer kann Strings, Daten, Zeiten, Zeitspannen, Zahlen und Mengen in einem leichter lesbaren Format bearbeiten und anzeigen. Unten ist ein Beispiel für die Verwendung von Humanizer für komplexe Zeichenkettenmanipulationen. Beachten Sie, dass Humanizer nicht Teil der .NET-Standardbibliothek ist und die Installation des NuGet-Pakets `Humanizer` erfordert.

Zuerst installieren Sie Humanizer über NuGet:

```
Install-Package Humanizer
```

Danach können Sie es wie folgt verwenden:

```csharp
using Humanizer;

int dayDifference = 5;
string humanized = $"Die Veranstaltung war vor {dayDifference} Tagen.".Humanize();
Console.WriteLine(humanized);
// Abhängig von der Konfiguration und Kultur, mögliche Ausgabe: Die Veranstaltung war vor 5 Tagen.
```

Dieses Beispiel demonstriert die grundlegende Verwendung. Humanizer unterstützt eine breite Palette von Funktionalitäten, die auf Strings, Daten, Zahlen und mehr angewendet werden können, was Ihre Anwendungen zugänglicher und intuitiver macht.
