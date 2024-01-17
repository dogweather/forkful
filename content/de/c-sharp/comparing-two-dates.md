---
title:                "Vergleich von zwei Datumsangaben"
html_title:           "C#: Vergleich von zwei Datumsangaben"
simple_title:         "Vergleich von zwei Datumsangaben"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Was & Warum?
Beim Programmieren muss man oft zwei verschiedene Termine oder Daten miteinander vergleichen, um beispielsweise zu überprüfen, welches Datum früher oder später ist. Dies ist eine wichtige Funktion, die Programmierer nutzen, um logische Entscheidungen zu treffen oder Datensätze zu sortieren.

## Wie geht's?
Der Vergleich von zwei Daten in C# ist relativ einfach. Wir können die integrierte Methode `Compare()` nutzen, um zwei `DateTime`-Objekte zu vergleichen.
```C#
DateTime date1 = new DateTime(2021, 01, 01);
DateTime date2 = new DateTime(2021, 01, 15);
int result = DateTime.Compare(date1, date2);
Console.WriteLine(result); // Output: -1 (date1 is earlier than date2)
```

## Tiefere Einblicke
Die `Compare()` Methode vergleicht die angegebenen Daten auf Basis von Zeit und Datum. Alternativ können wir auch die `Day`, `Month` und `Year` Eigenschaften nutzen, um nur einen Teil des Datums zu vergleichen.
Eine weitere Möglichkeit ist die Verwendung der `CompareTo()` Methode, die auch negative, 0 oder positive Werte zurückgibt, abhängig davon, ob das erste Datum vor, gleich oder nach dem zweiten Datum liegt. 

## Siehe auch
Weitere Informationen zur `Compare()` und `CompareTo()` Methode sowie zur Verwendung von Datumsvergleichen in C# finden Sie hier:
- [Microsoft Dokumentation](https://docs.microsoft.com/de-de/dotnet/api/system.datetime.compare?view=net-5.0)
- [Tutorial zum Vergleichen von Datumsangaben in C#](https://www.c-sharpcorner.com/article/how-to-compare-two-dates-in-c-sharp/)