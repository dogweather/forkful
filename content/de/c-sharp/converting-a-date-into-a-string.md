---
title:                "Ein Datum in einen String umwandeln"
html_title:           "C#: Ein Datum in einen String umwandeln"
simple_title:         "Ein Datum in einen String umwandeln"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum

Es gibt mehrere Gründe, warum man das Datum in eine Zeichenfolge umwandeln möchte. Dies kann hilfreich sein, um das Datum lesbarer zu gestalten oder es in einem bestimmten Format für die Datenspeicherung oder -übertragung zu konvertieren.

## Wie geht man vor

Um das Datum in eine Zeichenfolge umzuwandeln, gibt es verschiedene Techniken in C#. Eine Möglichkeit ist die Verwendung der Methode `ToString()`, die in der DateTime-Klasse enthalten ist. Hier ist ein Beispiel:

```C#
DateTime date = new DateTime(2020, 07, 15);
string dateString = date.ToString("dd.MM.yyyy");
Console.WriteLine(dateString);
```

Ausgabe: `15.07.2020`

In diesem Beispiel wird mit Hilfe der `ToString()`-Methode das Datum in das gewünschte Format `dd.MM.yyyy` (Tag.Monat.Jahr) umgewandelt. Die Methode ermöglicht es, das Datum in verschiedenen Formaten wie zum Beispiel `MM/dd/yyyy` (Monat/Tag/Jahr) oder `yyyy/MM/dd` (Jahr/Monat/Tag) auszugeben.

Eine weitere Möglichkeit ist die Verwendung der String-Interpolation, die in C# 6.0 eingeführt wurde. Dabei werden Variablen direkt in die Zeichenfolge eingefügt, indem sie in geschweifte Klammern gesetzt werden. Hier ist ein Beispiel:

```C#
DateTime date = new DateTime(2020, 07, 15);
string dateString = $"{date.Day}/{date.Month}/{date.Year}";
Console.WriteLine(dateString);
```

Ausgabe: `15/07/2020`

In diesem Fall werden die Werte `Day`, `Month` und `Year` aus dem `DateTime`-Objekt verwendet und in die Zeichenfolge eingefügt.

## Tiefergehende Informationen

Die Verwendung der `ToString()`-Methode ist die Standardmethode, um ein Datum in eine Zeichenfolge umzuwandeln. Dabei können verschiedene Formatierungsoptionen verwendet werden, um das Ausgabeformat anzupassen. Diese sind in der Dokumentation von Microsoft im Detail erklärt.

Eine wichtige Sache zu beachten ist auch die Lokalität oder Region, in der die Anwendung ausgeführt wird. Das hat einen Einfluss auf die Darstellung des Datums, da unterschiedliche Kulturen verschiedene Datumsformate verwenden. Um dieses Problem zu lösen, kann das `CultureInfo`-Objekt verwendet werden, um die Kultur zu spezifizieren, in der die Anwendung ausgeführt wird.

## Siehe auch

- [DateTime.ToString() Methode (System)](https://docs.microsoft.com/de-de/dotnet/api/system.datetime.tostring)
- [Verwendung der String-Interpolation (C#-Programmierhandbuch)](https://docs.microsoft.com/de-de/dotnet/csharp/language-reference/tokens/interpolated)
- [Kulturinformationen in .NET (Microsoft-Dokumentation)](https://docs.microsoft.com/de-de/dotnet/standard/globalization-localization/)