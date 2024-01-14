---
title:                "C#: Umwandeln eines Datums in einen String"
simple_title:         "Umwandeln eines Datums in einen String"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Konvertieren von Datum in einen String ist eine häufige Aufgabe in der C# Programmierung. Es kann nützlich sein, um Datum in einem lesbareren Format anzuzeigen oder als Teil einer Datenverarbeitungsfunktion. In diesem Blogbeitrag werden wir darüber sprechen, wie man ein Datum in einen String konvertiert und warum es wichtig ist, dieses Konzept zu verstehen.

## Wie es geht

Es gibt mehrere Möglichkeiten, ein Datum in einen String zu konvertieren. Hier sind zwei Beispiele mit jeweils unterschiedlichen Ausgabeformaten:

```C#
// Beispiel 1 - Konvertierung in ISO-Format
DateTime date = new DateTime(2021, 2, 14); // 14. Februar 2021
string isoDate = date.ToString("yyyy-MM-dd"); // Konvertiert in "2021-02-14"
Console.WriteLine(isoDate); // Gibt "2021-02-14" aus

// Beispiel 2 - Konvertierung in deutsches Datum
DateTime date = new DateTime(2021, 2, 14); // 14. Februar 2021
string deDate = date.ToString("dd.MM.yyyy"); // Konvertiert in "14.02.2021"
Console.WriteLine(deDate); // Gibt "14.02.2021" aus
```

In diesen Beispielen verwenden wir die `ToString()` Methode der DateTime Klasse, um das Datum in einen String zu konvertieren. Dabei können wir das gewünschte Ausgabeformat angeben, indem wir spezielle Zeichen in Anführungszeichen in der Methode `ToString()` verwenden. Zum Beispiel bedeutet "yyyy" das Jahr, "MM" den Monat und "dd" den Tag. Eine vollständige Liste der verfügbaren Formatierungszeichen findet man in der [DateTime Dokumentation](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings).

Eine weitere Möglichkeit, ein Datum in einen String zu konvertieren, ist die Verwendung der `string.Format()` Methode:

```C#
// Beispiel 3 - String.Format Methode
DateTime date = new DateTime(2021, 2, 14); // 14. Februar 2021
string dateStr = string.Format("Das Datum ist {0:D}", date);
// Konvertiert in "Das Datum ist Sonntag, 14. Februar 2021"
Console.WriteLine(dateStr); // Gibt "Das Datum ist Sonntag, 14. Februar 2021" aus
```

In diesem Beispiel verwenden wir Platzhalter `{0:D}` in der `string.Format()` Methode, um das Datum in einem bestimmten Format anzuzeigen. Auch hier gibt es verschiedene Platzhalter, die verwendet werden können, um das Datum in verschiedenen Formaten darzustellen. Eine Liste der verfügbaren Platzhalter findet man auch in der [DateTime Dokumentation](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings).

## Tiefergehende Informationen

Das Konvertieren von Datum in einen String kann komplex sein, da es viele unterschiedliche Formatierungsmöglichkeiten gibt und es auf die speziellen Anforderungen der Anwendung ankommt. Es ist wichtig, die verfügbaren Optionen und Platzhalter zu verstehen, um das Datum genau in dem gewünschten Format darzustellen.

Außerdem sollte man auch beachten, dass das Datum in C# durch die Kultur oder Region beeinflusst sein kann. Zum Beispiel wird das Datum in Deutschland anders formatiert als in den USA. Daher ist es wichtig, die Kultur- oder Regionsinformationen bei der Konvertierung von Datum in einen String zu berücksichtigen.

## Siehe auch

- [DateTime Dokumentation](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings)
- [Kultur und Region in C#](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo?view=net-5.0)