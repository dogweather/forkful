---
title:                "C#: Verketten von Zeichenfolgen"
simple_title:         "Verketten von Zeichenfolgen"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum

Die Verkettung von Zeichenfolgen ist ein wichtiges Konzept in der Programmierung, insbesondere in C#. Es ermöglicht Programmierern, mehrere Zeichenfolgen zu einer neuen Zeichenfolge zusammenzufügen, was sehr nützlich ist, um dynamische Textinhalte zu erstellen. In diesem Blogbeitrag werden wir uns genauer mit der Verkettung von Zeichenfolgen in C# beschäftigen und lernen, wie man sie in der Praxis anwendet.

## Wie man es macht

Die Verkettung von Zeichenfolgen in C# kann mit dem "+" Operator durchgeführt werden. Dieser Operator fügt einfach zwei vorhandene Zeichenfolgen zusammen und gibt eine neue Zeichenfolge zurück. Schauen wir uns ein Beispiel an:

```C#
string str1 = "Hallo";
string str2 = "Welt";
string str3 = str1 + str2;

Console.WriteLine(str3);
```

Dieser Code gibt die Zeichenfolge "HalloWelt" aus, da str1 und str2 miteinander verkettet wurden.

Man kann auch mehrere Zeichenfolgen in einer Reihe verketten, indem man den "+" Operator mehrmals verwendet. Zum Beispiel:

```C#
string str1 = "Hallo";
string str2 = " ";
string str3 = "Welt";
string str4 = "!";

string str5 = str1 + str2 + str3 + str4;

Console.WriteLine(str5);
```

Dieser Code gibt die Zeichenfolge "Hallo Welt!" aus.

Eine weitere Möglichkeit, Zeichenfolgen zu verkettet, ist die Verwendung der `string.Format()` Methode. Diese Methode akzeptiert eine Formatierungsvorlage und eine beliebige Anzahl von zu formatierenden Parametern. Ein Beispiel:

```C#
string name = "Maria";
int age = 25;

string greeting = string.Format("Hallo {0}, du bist {1} Jahre alt.", name, age);

Console.WriteLine(greeting);
```

Dieser Code gibt die Zeichenfolge "Hallo Maria, du bist 25 Jahre alt." aus.

## Tiefere Einblicke

Beim Verketten von Zeichenfolgen in C# sollte man beachten, dass dies eine ineffiziente Methode sein kann, insbesondere wenn eine große Anzahl von Zeichenfolgen verkettet wird. Jedes Mal, wenn der "+" Operator verwendet wird, wird eine neue Zeichenfolge im Speicher erstellt, was zu einer höheren Speichernutzung führen kann. Um dieses Problem zu lösen, gibt es die `StringBuilder` Klasse, die speziell für die Erstellung und Verwaltung von Zeichenfolgen optimiert ist.

Ein weiteres wichtiges Konzept beim Verketten von Zeichenfolgen ist die Verwendung von Escape-Sequenzen. Diese ermöglichen es, bestimmte Sonderzeichen wie Anführungszeichen oder Zeilenumbrüche in Zeichenfolgen zu verwenden, die sonst zu Syntaxfehlern führen könnten. Zum Beispiel:

```C#
string str = "Ich m\u00F6chte \" Hallo \" sagen.";
```

In diesem Beispiel wird das Anführungszeichen in der Zeichenfolge durch die Escape-Sequenz `\"` gekennzeichnet.

## Siehe auch

- [Microsoft Documentation on String Concatenation in C#](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/)
- [C# Tutorials - String Concatenation](https://www.tutorialspoint.com/csharp/csharp_string_concatenation.htm)
- [C# Escape Sequences](https://www.tutorialsteacher.com/csharp/csharp-escape-sequence)