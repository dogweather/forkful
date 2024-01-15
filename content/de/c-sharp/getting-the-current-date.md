---
title:                "Das aktuelle Datum erhalten"
html_title:           "C#: Das aktuelle Datum erhalten"
simple_title:         "Das aktuelle Datum erhalten"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum
Das aktuelle Datum ist ein wichtiger Teil jeder Anwendung oder Software, da es oft verwendet wird, um Benutzern genaue Zeitinformationen und Zeitstempel zu liefern.

## Wie man das aktuelle Datum bekommt
Um das aktuelle Datum in C# zu erhalten, gibt es verschiedene Möglichkeiten. Eine Möglichkeit besteht darin, die DateTime.Now-Eigenschaft zu verwenden, die die aktuelle lokale Zeit auf dem System zurückgibt. Hier ist ein Beispielcode mit Ausgabe:

```C#
DateTime now = DateTime.Now;
Console.WriteLine(now);
// Ausgabe: 19.03.2021 10:05:00
```

Eine andere Möglichkeit besteht darin, die DateTime.Today-Eigenschaft zu verwenden, die das aktuelle Datum ohne Uhrzeit zurückgibt. Hier ist ein Beispielcode mit Ausgabe:

```C#
DateTime today = DateTime.Today;
Console.WriteLine(today);
// Ausgabe: 19.03.2021 00:00:00
```

Es gibt auch die Möglichkeit, das aktuelle Datum mit einer bestimmten Formatierung anzuzeigen, indem die ToString-Methode verwendet wird. Hier ist ein Beispielcode mit Ausgabe, die das aktuelle Datum in dem angegebenen Format (TT.MM.JJJJ) anzeigt:

```C#
DateTime now = DateTime.Now;
Console.WriteLine(now.ToString("dd.MM.yyyy"));
// Ausgabe: 19.03.2021
```

## Tiefere Einblicke
Das aktuelle Datum in C# basiert auf der Klasse DateTime, die in der .NET-Bibliothek enthalten ist. Diese Klasse bietet auch viele nützliche Methoden, um das aktuelle Datum in verschiedenen Formaten anzuzeigen oder um bestimmte Eigenschaften wie Tag, Monat oder Jahr abzurufen. Außerdem gibt es in der .NET-Bibliothek auch die Klasse DateTimeOffset, die zusätzlich zur DateTime-Klasse die Zeitzone berücksichtigt.

## Siehe auch
- Dokumentation zur DateTime-Klasse: https://docs.microsoft.com/de-de/dotnet/api/system.datetime?view=net-5.0
- Dokumentation zur DateTimeOffset-Klasse: https://docs.microsoft.com/de-de/dotnet/api/system.datetimeoffset?view=net-5.0