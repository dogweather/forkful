---
title:                "C#: Suchen und Ersetzen von Text"
simple_title:         "Suchen und Ersetzen von Text"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Warum

Wenn du mit C# programmierst, wirst du häufig Änderungen an deinem Code vornehmen müssen. Eine der häufigsten Aufgaben ist das Suchen und Ersetzen von Text. Dies kann zeitaufwändig sein, wenn du jedes Vorkommen des Textes manuell ändern musst. Glücklicherweise gibt es in C# eine Möglichkeit, dies automatisch zu erledigen. Im folgenden Artikel erfährst du, wie du effizient Text suchen und ersetzen kannst.

## Wie geht das?

Um Text in C# programmatisch zu suchen und zu ersetzen, müssen wir die Methode "Replace" der string Klasse verwenden. Diese Methode nimmt zwei Parameter an - den zu ersetzenden Text und den neuen Text. Das folgende Beispiel zeigt, wie man alle Vorkommen des Wortes "Hund" in einem Satz durch "Katze" ersetzen kann:

```C#
string satz = "Ich habe einen Hund, und er ist mein bester Freund.";
string neuerSatz = satz.Replace("Hund", "Katze");

Console.WriteLine(neuerSatz);
```

Die Ausgabe dieses Codes wäre: "Ich habe eine Katze, und er ist mein bester Freund." Wie du sehen kannst, wurde das Wort "Hund" durch "Katze" ersetzt.

Du kannst auch eine weitere Option in der "Replace" Methode verwenden, um anzugeben, ob die Groß- und Kleinschreibung berücksichtigt werden soll. Wenn du beispielsweise möchtest, dass nur das Wort "hund" und nicht "Hund" ersetzt wird, kannst du das folgende Code-Beispiel verwenden:

```C#
string satz = "Ich habe einen Hund, und er ist mein bester Freund.";
string neuerSatz = satz.Replace("hund", "Katze", StringComparison.OrdinalIgnoreCase);

Console.WriteLine(neuerSatz);
```

Die Ausgabe wäre in diesem Fall gleich, aber "Hund" würde weiterhin großgeschrieben bleiben.

## Tiefentauchen

Die "Replace" Methode hat noch einige weitere nützliche Optionen, wie z.B. die Angabe einer maximalen Anzahl an Ersetzungen oder die Angabe eines variablen Startpunkts zum Suchen des Textes. Diese Optionen können in bestimmten Fällen sehr praktisch sein, insbesondere wenn du mit großen Textmengen arbeitest.

Eine weitere Möglichkeit, Text in C# zu suchen und zu ersetzen, ist die Verwendung von regulären Ausdrücken. Diese erweiterte Suche ermöglicht es dir, komplexere Muster zu erkennen und zu ersetzen. Weitere Informationen zu RegEx in C# findest du in [diesem Artikel](https://www.c-sharpcorner.com/article/searching-string-with-regular-expression/).

## Siehe auch

- [string.Replace Methode in der offiziellen Dokumentation](https://docs.microsoft.com/de-de/dotnet/api/system.string.replace?view=net-5.0)
- [Reguläre Ausdrücke in C# - Ein Tutorial](https://www.c-sharpcorner.com/article/regular-expression-in-C-Sharp/)