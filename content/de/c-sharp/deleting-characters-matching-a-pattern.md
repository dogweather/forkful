---
title:                "C#: Löschen von Zeichen, die einem Muster entsprechen"
simple_title:         "Löschen von Zeichen, die einem Muster entsprechen"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Warum

Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, ist eine nützliche Fähigkeit im Bereich der Programmierung. Es kann dabei helfen, unerwünschte oder fehlerhafte Daten zu bereinigen und ordnungsgemäße Formate sicherzustellen. In diesem Blog-Beitrag werden wir uns genauer damit beschäftigen, wie man in C# Zeichen löschen kann, die einem bestimmten Muster entsprechen.

# Wie geht man vor?

Um in C# Zeichen zu löschen, die einem bestimmten Muster entsprechen, kann man die Methode `Replace()` verwenden. Diese Methode erwartet zwei Parameter: das Muster, das ersetzt werden soll, und durch was es ersetzt werden soll. In unserem Beispiel wollen wir alle Vokale aus einem gegebenen Wort löschen. Wir verwenden daher das Muster `[aeiou]` und lassen es durch einen leeren String ersetzen.

```C#
string word = "Hallo";
string modifiedWord = word.Replace("[aeiou]", "");

Console.WriteLine(modifiedWord);
```

Die Ausgabe unseres Codes wird `Hll` sein, da alle Vokale aus dem Wort "Hallo" gelöscht wurden.

# Tiefergehende Informationen

Die `Replace()` Methode kann auch mit regulären Ausdrücken verwendet werden. Reguläre Ausdrücke sind Muster, die verwendet werden, um Text zu durchsuchen und damit bestimmte Zeichen zu finden oder zu ersetzen. Sie können sehr hilfreich sein, um komplexe Muster zu implementieren und das Löschen von Zeichen zu vereinfachen. In unserem obigen Beispiel könnten wir beispielsweise auch alle Zahlen aus einem String löschen, indem wir das Muster `[0-9]` verwenden.

# Siehe auch

- [C# String.Replace() Methode] (https://docs.microsoft.com/en-us/dotnet/api/System.String.Replace?view=netcore-3.1)
- [Reguläre Ausdrücke in C#] (https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expression-language-quick-reference)
- [C# Tutorials] (https://www.c-sharpcorner.com/learn/c-sharp-tutorials/)