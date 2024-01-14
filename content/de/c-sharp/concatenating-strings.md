---
title:    "C#: Zeichenfolgen verknüpfen"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

Warum Strings in C# konkatenieren?

String-Konkatenation ist eine grundlegende und wichtige Funktion beim Programmieren in C#. Strings sind eine Datenstruktur, die häufig für die Verarbeitung und Darstellung von Text verwendet wird. Die Fähigkeit, Strings effektiv zu konkatenieren, ist daher unerlässlich, um komplexe und dynamische Programmlogik umzusetzen.

Wie wird es gemacht?

Die Konkatenation von Strings in C# ist relativ einfach und erfordert nur die Verwendung des "+" Operators. Hier ist ein Beispielcode, der zwei Strings zusammenfügt:

```C#
string firstName = "Max";
string lastName = "Müller";
string fullName = firstName + " " + lastName;
Console.WriteLine(fullName);
```

Die Ausgabe dieses Codes wäre "Max Müller". Wie Sie sehen können, können nicht nur einzelne Zeichenfolgen, sondern auch Variablen und Ausdrücke miteinander konkateniert werden.

Tiefergehende Erklärung

Bei der Konkatenation von Strings in C# müssen Sie sich bewusst sein, dass der "+" Operator hinter den Kulissen einen anderen Operator namens "String.Concat" verwendet. Dieser Operator nimmt eine beliebige Anzahl von Argumenten entgegen und fügt sie zu einer einzigen Zeichenfolge zusammen. Daher können Sie auch mehr als zwei Strings miteinander konkatenieren, indem Sie einfach weitere Argumente dem "+" Operator hinzufügen.

Ein weiterer wichtiger Punkt bei der String-Konkatenation ist die Performance. Da Strings in C# unveränderlich sind, müssen bei jedem Konkatenationsvorgang neue Strings erstellt werden, was zu einem höheren Speicherverbrauch führen kann. Daher ist es oft effizienter, die Klasse "StringBuilder" zu verwenden, um Strings zusammenzufügen, insbesondere wenn Sie eine größere Anzahl von Zeichenfolgen konkatenieren müssen.

Siehe auch

- Offizielle Dokumentation zu String-Konkatenation in C#: https://docs.microsoft.com/de-de/dotnet/csharp/programming-guide/strings/#concatenation
- Ein ausführliches Tutorial zur String-Konkatenation in C#: https://www.tutorialspoint.com/csharp/csharp_string_concatenation.htm
- Weitere Informationen zur Klasse "StringBuilder": https://www.dotnetperls.com/stringbuilder