---
title:    "C#: Umwandlung eines Datums in eine Zeichenfolge"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Umwandeln von Datum in einen String ist eine grundlegende Funktion, die jeder Programmierer in C# kennen sollte. Ob Sie ein Anfänger sind oder ein erfahrener Entwickler, es ist wichtig zu verstehen, wie man dies in C# richtig macht. In diesem Blogbeitrag werden wir uns genauer damit beschäftigen, warum wir Datumsangaben in Strings umwandeln müssen und wie man dies in C# effektiv durchführt.

## Wie man Datum in String konvertiert

Um in C# ein Datum in einen String zu konvertieren, benötigen wir die Methode "ToString()". Diese Methode gehört zu jeder Datum- und Uhrzeitklasse in C# und ermöglicht es uns, ein Datum in verschiedenen Formaten als String auszugeben. Schauen wir uns ein Beispiel an:

```C#
using System;

DateTime date = new DateTime(2020, 6, 23);
string dateString = date.ToString("dd/MM/yyyy");

Console.WriteLine(dateString); // output: 23/06/2020
```

In diesem Beispiel haben wir ein DateTime-Objekt "date" erstellt, das den 23. Juni 2020 darstellt. Dann haben wir die Methode "ToString()" verwendet, um dieses Datum in einem benutzerdefinierten Format ("dd/MM/yyyy") als String auszugeben. Sie können das Format nach Ihren Wünschen anpassen, indem Sie verschiedene Parameter in die Methode einfügen. Weitere Informationen zu den verfügbaren Formaten finden Sie in der offiziellen C#-Dokumentation.

## Tiefer Einblick

Die Konvertierung von Datum in String mag einfach erscheinen, aber es gibt einige Dinge, die man beachten sollte. Zum Beispiel kann es in Fällen, in denen verschiedene Sprachen und Kulturen eine Rolle spielen, zu Problemen kommen. Stellen Sie sich vor, Sie möchten das Datum in einigen Ländern als "MM/dd/yyyy" und in anderen als "dd/MM/yyyy" ausgeben. Um dies richtig zu machen, müssen Sie die richtige CultureInfo verwenden und diese in der ToString()-Methode festlegen. Ein weiterer Faktor ist die Verwendung von DateTime-Formatierungszeichenfolgen, die einige spezielle Zeichen enthalten, um das Datum in der gewünschten Form auszugeben. Es ist wichtig, diese sorgfältig auszuwählen, um die beste Darstellung des Datums zu erreichen.

## Siehe auch

- Microsoft C# Dokumentation: [ToString() Method](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.tostring)
- Codecademy: [C# Date and Time](https://www.codecademy.com/learn/learn-c-sharp/modules/csharp-date-and-time)
- YouTube Video: [C# Tutorial - DateTime Format](https://www.youtube.com/watch?v=WLTwATsKtFk)