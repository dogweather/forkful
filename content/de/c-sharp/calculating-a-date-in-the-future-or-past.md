---
title:    "C#: Eine Datum im Zukunft oder Vergangenheit berechnen"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Warum

Das Berechnen von Datumsangaben in der Zukunft oder Vergangenheit ist ein wesentlicher Bestandteil der Programmierung. Es ermöglicht uns, Termine und Fristen zu berücksichtigen und Abläufe in unseren Code einzubauen. In diesem Blogbeitrag werden wir uns ansehen, wie man mit C# ein Datum in der Zukunft oder Vergangenheit berechnet.

## So geht's

Um ein Datum in der Zukunft oder Vergangenheit zu berechnen, können wir die DateTime-Klasse in C# verwenden. Diese Klasse bietet verschiedene Methoden und Eigenschaften, die uns bei der Manipulation von Datumsangaben helfen.

Hier ist ein Beispiel, wie man ein Datum in der Zukunft berechnet, indem man 30 Tage zu einem bestehenden Datum hinzufügt:

```C#
DateTime myDate = DateTime.Now;
DateTime futureDate = myDate.AddDays(30);
Console.WriteLine(futureDate); // Ausgabe: 14.09.2021
```

Und so sieht es aus, wenn wir ein Datum in der Vergangenheit berechnen, indem wir 2 Monate von einem bestehenden Datum abziehen:

```C#
DateTime myDate = DateTime.Now;
DateTime pastDate = myDate.AddMonths(-2);
Console.WriteLine(pastDate); // Ausgabe: 15.06.2021
```

Wie Sie sehen können, verwenden wir die DateTime.Now-Eigenschaft, um das aktuelle Datum und die aktuelle Uhrzeit zu erhalten. Dann verwenden wir die entsprechende Methode (AddDays oder AddMonths) und geben den Wert an, um das berechnete Datum zu erhalten.

Aber was ist, wenn wir eine bestimmte Anzahl von Jahren, Monaten oder Tagen zu einem Datum hinzufügen möchten, anstatt eine konkrete Zahl? Hier kommt die TimeSpan-Klasse ins Spiel. Diese Klasse repräsentiert einen Zeitraum und kann verwendet werden, um ein Datum zu manipulieren.

Hier ist ein Beispiel, wie wir 2 Jahre, 3 Monate und 10 Tage zu einem Datum hinzufügen:

```C#
DateTime myDate = DateTime.Now;
TimeSpan timeSpan = new TimeSpan(2, 3, 10);
DateTime futureDate = myDate + timeSpan;
Console.WriteLine(futureDate); // Ausgabe: 26.01.2024
```

## Tiefere Einblicke

Beim Manipulieren von Datumsangaben gibt es einige wichtige Dinge zu beachten. Zum Beispiel ist es wichtig, dass wir im Auge behalten, wie wir die Datumsangaben formatieren. Wenn wir ein Datum in verschiedenen Formaten ausgeben möchten, können wir die ToString-Methode verwenden und das gewünschte Format angeben.

Außerdem sollten wir beim Berechnen von Datumsangaben immer sicherstellen, dass wir auch Schaltjahre und Schaltsekunden berücksichtigen. Dazu können wir die DateTime.IsLeapYear-Methode verwenden, um zu überprüfen, ob ein bestimmtes Jahr ein Schaltjahr ist.

Es ist auch möglich, mit negativen Werten zu arbeiten, um ein Datum in der Vergangenheit zu berechnen. Wenn wir z.B. -1 als Argument für die AddDays-Methode angeben, erhalten wir das Datum vom Vortag.

## Siehe auch

- [Microsoft Dokumentation zu DateTime](https://docs.microsoft.com/de-de/dotnet/api/system.datetime?view=net-5.0)
- [Microsoft Dokumentation zu TimeSpan](https://docs.microsoft.com/de-de/dotnet/api/system.timespan?view=net-5.0)
- [Schaltjahre und Schaltsekunden in C# berechnen](https://www.geeksforgeeks.org/leap-year-program-in-c-sharp/)