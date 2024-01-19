---
title:                "Eine Zeichenkette interpolieren"
html_title:           "Arduino: Eine Zeichenkette interpolieren"
simple_title:         "Eine Zeichenkette interpolieren"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

String-Interpolation ist eine Methode, um Werte in Zeichenketten einzufügen. Programmierer verwenden sie, um den Code lesbarer und wartungsfreundlicher zu gestalten. 

## Wie geht denn das? 

Mit String-Interpolation kann in C# auf eine saubere und effiziente Weise Werte in Strings integriert werden. Hier ist ein einfacher Weg, es zu tun:

```C#
string name = "Hans";
string greeting = $"Hallo, {name}!";
Console.WriteLine(greeting);
```

In diesem Fall gibt der Code "Hallo, Hans!" aus.

## Tiefgehende Details

1. Historischer Kontext: String-Interpolation wurde in C# 6.0 eingeführt, um die String-Formatierung zu vereinfachen.

2. Alternativen: Vor C# 6.0 nutzten Entwickler manuell indizierte Platzhalter und `String.Format`. Hier ist ein Beispiel:

    ```C#
    string oldWay = String.Format("Hallo, {0}!", name);
    ```

3. Implementation: Intern übersetzt der Compiler interpolierte Strings in eine `String.Format`-Methode, sodass die Leistung mit dem älteren Stil vergleichbar ist. Tatsächlich ist das Hauptziel die Verbesserung der Lesbarkeit und Wartbarkeit.

## Siehe auch

Für zusätzliche Informationen, schauen Sie in die offizielle Dokumentation:
- [String Interpolation in C#](https://docs.microsoft.com/de-de/dotnet/csharp/language-reference/tokens/interpolated)
- [Was ist neu in C# 6.0](https://docs.microsoft.com/de-de/archive/blogs/csharpfaq/new-features-in-c-6)