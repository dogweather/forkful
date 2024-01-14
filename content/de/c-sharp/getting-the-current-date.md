---
title:                "C#: Das Aktuelle Datum erhalten"
simple_title:         "Das Aktuelle Datum erhalten"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Warum
Bevor wir uns in die Details des aktuellen Datums stürzen, lassen Sie mich Ihnen sagen, warum Sie das überhaupt wissen möchten. Das aktuelle Datum ist eine häufig verwendete Information in der Programmierung, besonders wenn es um die Verwaltung von Benutzereingaben oder die Verfolgung von Ereignissen geht.

# Wie geht das?
Um das aktuelle Datum in C# zu erhalten, gibt es eine Vielzahl von Optionen. Eine Möglichkeit ist die Verwendung der `DateTime`-Struktur, die in der .NET Framework-Klasse enthalten ist. Schauen wir uns ein Beispiel an:
```c#
DateTime now = DateTime.Now;
Console.WriteLine(now);
```
Dieses Beispiel verwendet die `Now`-Eigenschaft der `DateTime`-Struktur, um das aktuelle Datum und die Uhrzeit zu erhalten. Dies wird dann in der Konsole ausgegeben, so dass Sie das Ergebnis sehen können. Dieses Ergebnis wird in einem Format wie diesem angezeigt:
```
3/17/2021 10:00:00 AM
```
Wenn Sie nur das Datum ohne die Uhrzeit benötigen, können Sie die `Today`-Eigenschaft verwenden. Sie funktioniert auf die gleiche Weise wie die `Now`-Eigenschaft, gibt jedoch nur das Datum zurück. Hier ist ein Beispiel:
```c#
DateTime today = DateTime.Today;
Console.WriteLine(today);
```
Das Ergebnis würde in diesem Fall nur das Datum enthalten, ohne die Uhrzeit:
```
3/17/2021 12:00:00 AM
```
Eine andere Alternative ist die Verwendung der `DateTimeOffset`-Struktur, die es Ihnen ermöglicht, das Datum und die Uhrzeit in verschiedenen Zeitzonen zu erhalten. Hier ist ein Beispiel:
```c#
DateTimeOffset now = DateTimeOffset.Now;
Console.WriteLine(now);
```
Das Ergebnis hier enthält die vollständige Zeitzoneinformation, wie z.B. die Verschiebung von der UTC-Zeitzone:
```
3/17/2021 7:00:00 AM -07:00
```

# Tiefer Einblick
Jetzt wissen Sie, wie Sie das aktuelle Datum in C# erhalten. Aber wussten Sie auch, dass Sie das Datum und die Uhrzeit auch manipulieren können? Die `DateTime`- und `DateTimeOffset`-Strukturen bieten eine Vielzahl von Methoden, um das Datum und die Uhrzeit auf verschiedene Weise zu ändern. Sie können z.B. den Monat oder das Jahr ändern, einen bestimmten Tag hinzufügen oder subtrahieren oder sogar die Zeitzone anpassen.

Eine wichtige Sache zu beachten ist, dass die `DateTime`- und `DateTimeOffset`-Strukturen unveränderlich sind, was bedeutet, dass sie nicht direkt geändert werden können. Stattdessen gibt jede Methode, die das Datum ändert, eine neue Instanz zurück. Hier ist ein Beispiel, das das Hinzufügen von 5 Tagen zum aktuellen Datum zeigt:
```c#
DateTime newDate = DateTime.Now.AddDays(5);
Console.WriteLine(newDate);
```
Das Ergebnis wäre das Datum, das 5 Tage in der Zukunft liegt:
```
3/22/2021 10:00:00 AM
```

# Siehe auch
- [Microsoft Docs - DateTime Struktur](https://docs.microsoft.com/de-de/dotnet/api/system.datetime?view=net-5.0)
- [Microsoft Docs - DateTimeOffset Struktur](https://docs.microsoft.com/de-de/dotnet/api/system.datetimeoffset?view=net-5.0)
- [Tutorialspoint - C# Datetime Manipulation](https://www.tutorialspoint.com/csharp/csharp_datetime_manipulation.htm)