---
title:                "Zeichenketten interpolieren"
date:                  2024-01-20T17:50:41.544567-07:00
model:                 gpt-4-1106-preview
simple_title:         "Zeichenketten interpolieren"

category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
String-Interpolation in C# ermöglicht es, Variablenwerte nahtlos in Strings einzubetten. Programmierer nutzen das, um lesbare und wartbare Codes zu erstellen, indem sie dynamische Inhalte in Strings einflechten.

## Anleitung:
Zuerst das Grundprinzip. Mit dem Dollarzeichen (`$`) vor einem String signalisieren Sie C#, dass die geschweiften Klammern (`{}`) innerhalb des Strings Variablen oder Ausdrücke enthalten, die interpoliert werden sollen.

```C#
string name = "Johannes";
int age = 28;
string greeting = $"Hallo, mein Name ist {name} und ich bin {age} Jahre alt.";
Console.WriteLine(greeting);
// Ausgabe: Hallo, mein Name ist Johannes und ich bin 28 Jahre alt.
```

Man kann auch Berechnungen direkt einfügen:

```C#
int a = 5;
int b = 10;
string calculation = $"Fünf plus zehn ist gleich {a + b}.";
Console.WriteLine(calculation);
// Ausgabe: Fünf plus zehn ist gleich 15.
```

Und so funktioniert es mit Objekten:

```C#
DateTime now = DateTime.Now;
string timeStamp = $"Derzeit ist es {now.Hour}:{now.Minute} Uhr am {now.Day}.{now.Month}.{now.Year}.";
Console.WriteLine(timeStamp);
// Ausgabe zum Beispiel: Derzeit ist es 14:30 Uhr am 25.3.2023.
```

## Tiefgang:
String-Interpolation ist seit C# 6.0 Teil der Sprache, erschienen 2015, und löste die Formatierung mit `String.Format()` ab – allerdings ohne es vollständig zu ersetzen. Die Lesbarkeit und Kürze des interpolierten Strings sind klare Vorteile.

Hier sind Format-Alternativen in C#:

- `String.Format()`: Traditionelle Methode, Platzhalter mit Indizes zu verwenden.
  ```C#
  string oldSchool = String.Format("Hallo, ich bin {0} und {1} Jahre alt.", name, age);
  ```
- `StringBuilder`: Gut für häufige Modifikation großer Strings.
- `Concat()`, `Join()`: Zum Zusammenfügen von Strings.

Bezüglich der Implementierung konvertiert der Compiler interpolierte Strings in `String.Format()` Aufrufe. Jede geschweifte Klammer wird zum Platzhalter. Deshalb sind die Leistungscharakteristiken ähnlich. 

Außerdem, immer dran denken: `{` und `}` in interpolierte Strings direkt einbetten geht nicht ohne weiteres, da sie spezielle Zeichen sind. Für Literal `{` oder `}`, benutzen Sie `{{` oder `}}`.

## Weiterführendes:
- Microsoft Dokumentation über String-Interpolation: [docs.microsoft.com/de-de/dotnet/csharp/language-reference/tokens/interpolated](https://docs.microsoft.com/de-de/dotnet/csharp/language-reference/tokens/interpolated)
- Mehr zu `String.Format()`: [docs.microsoft.com/de-de/dotnet/api/system.string.format](https://docs.microsoft.com/de-de/dotnet/api/system.string.format)
- Performance-Aspekte: [csharpindepth.com/Articles/Strings](https://csharpindepth.com/Articles/Strings)
