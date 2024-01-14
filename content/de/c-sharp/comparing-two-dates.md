---
title:                "C#: Vergleich von zwei Daten"
programming_language: "C#"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum

Das Vergleichen von zwei Daten ist ein häufiger Vorgang in der Programmierung. Es kann dazu beitragen, bestimmte Aktionen basierend auf dem Ergebnis des Vergleichs durchzuführen oder um herauszufinden, ob ein bestimmtes Ereignis in der Vergangenheit liegt.

## Anleitung

Um zwei Daten in C# zu vergleichen, können wir die "Compare" -Methode der "DateTime" -Klasse verwenden. Hier ist ein Beispielcode, der die Verwendung dieser Methode zeigt:

```C#
DateTime date1 = new DateTime(2021, 3, 18);
DateTime date2 = new DateTime(2021, 3, 17);

int result = DateTime.Compare(date1, date2);
if (result > 0)
{
    Console.WriteLine("Date 1 is later than Date 2");
}
else if (result < 0)
{
    Console.WriteLine("Date 1 is earlier than Date 2");
}
else
{
    Console.WriteLine("Date 1 is equal to Date 2");
}
```

Die Ausgabe dieses Codes würde lauten: "Date 1 is later than Date 2". Dies geschieht, weil der 18. März 2021 später im Kalender liegt als der 17. März 2021.

## Tiefergehende Einblicke

Die "Compare" -Methode gibt einen Integer-Wert zurück, der folgende Bedeutungen hat:

- Wenn der Wert größer als 0 ist, ist das erste Datum später als das zweite Datum.
- Wenn der Wert kleiner als 0 ist, ist das erste Datum früher als das zweite Datum.
- Wenn der Wert gleich 0 ist, sind beide Daten gleich.

Zusätzlich können wir auch die "Equals" -Methode verwenden, um zu überprüfen, ob zwei Daten genau gleich sind. Es gibt auch andere Methoden, die beim Vergleichen von Daten hilfreich sein können, wie zum Beispiel "CompareTo" und "EqualsExact".

## Siehe auch

- [Offizielle Dokumentation über die "DateTime" -Klasse von Microsoft](https://docs.microsoft.com/de-de/dotnet/api/system.datetime?view=net-5.0)
- [Tutorial zur Arbeit mit Datum und Uhrzeit in C#](https://www.tutorialspoint.com/csharp/csharp_date_time.htm)
- [Blogbeitrag über das Vergleichen von Daten in C#](https://www.c-sharpcorner.com/blogs/compare-dates-in-c-sharp)