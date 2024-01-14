---
title:                "C#: Strings verknüpfen"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum

Wenn Sie schon eine Weile programmieren, haben Sie wahrscheinlich schon von der Konkatenation von Zeichenfolgen gehört. Aber warum ist es so wichtig? Nun, das Verbinden von Zeichenfolgen ist eine wesentliche Fähigkeit im C#-Programmieren, da es uns ermöglicht, mehrere Zeichenfolgen zu einem einzigen Text zu kombinieren. Dies kann besonders nützlich sein, wenn wir Benutzereingaben oder Daten aus verschiedenen Variablen zusammenführen möchten, um sie in einer einzigen Ausgabe anzuzeigen.

## Wie geht das?

Um Zeichenfolgen in C# zu verbinden, verwenden wir den `+` Operator, der als Verknüpfungsoperator bezeichnet wird. Hier ist ein Beispielcode mit einer einfachen Zeichenfolgenverbindung:

```C#
string str1 = "Hallo";
string str2 = "Welt";
string result = str1 + " " + str2;
Console.WriteLine(result);
```
Die Ausgabe wäre: "Hallo Welt". Wie Sie sehen können, haben wir mithilfe des `+` Operators die beiden Zeichenfolgen miteinander verbunden und ein Leerzeichen dazwischen eingefügt, um ein sinnvolles Ergebnis zu erzielen.

Ein weiteres Beispiel, um die Nützlichkeit von Zeichenfolgenverbindungen zu demonstrieren, ist die Zusammenführung von Benutzereingaben und Variablen. Angenommen, wir erstellen ein einfaches Programm, das den Namen und das Alter des Benutzers abfragt und dann eine personalisierte Begrüßung ausgibt:

```C#
Console.WriteLine("Wie ist dein Name?");
string name = Console.ReadLine();
Console.WriteLine("Wie alt bist du?");
int alter = Convert.ToInt32(Console.ReadLine());
string ausgabe = "Hallo " + name + ", du bist " + alter + " Jahre alt.";
Console.WriteLine(ausgabe);
```
Die Ausgabe könnte zum Beispiel lauten: "Hallo Lisa, du bist 25 Jahre alt."

## Tiefer eintauchen

Jetzt wissen Sie, wie man Zeichenfolgen in C# verbindet, aber es gibt noch einige weitere wichtige Dinge, die Sie darüber wissen sollten. Zunächst ist es wichtig zu beachten, dass der `+` Operator genau wie bei arithmetischen Operationen eine Reihenfolge der Ausführung hat. Dies bedeutet, dass er von links nach rechts arbeitet und zuerst die Zeichenfolgen auf der linken Seite verbindet, bevor er mit denen auf der rechten Seite fortfährt.

Außerdem gibt es in C# die `string.Format()` Methode, die es uns ermöglicht, Zeichenfolgen auf eine formatierte Art und Weise zu verbinden. Dies kann besonders nützlich sein, wenn wir komplexe Ausgaben erstellen wollen, indem wir Variablen innerhalb eines Textes platzieren.

## Siehe auch

- [MDN Web Docs: String Koncatenation](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/koncatenieren)
- [Microsoft Docs: Zeichenfolgenverbindungen](https://docs.microsoft.com/de-de/dotnet/csharp/how-to/concatenate-multiple-strings)