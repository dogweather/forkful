---
title:    "C#: Das aktuelle Datum erhalten"
keywords: ["C#"]
---

{{< edit_this_page >}}

# Warum

Das Abrufen des aktuellen Datums ist ein häufiger Bestandteil jeder Programmiersprache, einschließlich C #. Das aktuelle Datum wird häufig benötigt, um Analyse- oder Protokolldateien zu erstellen, Zeitstempel für bestimmte Aktionen zu erstellen oder Benutzern das aktuelle Datum anzuzeigen.

# Wie man das aktuelle Datum in C # erhält

Das Abrufen des aktuellen Datums in C # ist dank der DateTime-Klasse einfach zu erreichen. Hier ist ein Beispiel, wie man das aktuelle Datum in einem Datum/Zeit-Objekt speichert:

```C#
DateTime now = DateTime.Today;
```

Um das Datum in einem bestimmten Format anzuzeigen, kann die ToString() Methode verwendet werden. Hier ist ein Beispiel, wie man das Datum im Format "TT/MM/JJJJ" anzeigen lassen kann:

```C#
string dateString = now.ToString("dd/MM/yyyy");
Console.WriteLine(dateString); //Ausgabe: 10/08/2021
```

Es gibt viele verschiedene Möglichkeiten, das Datum in verschiedenen Formaten anzuzeigen. Schauen wir uns nun an, wie man das aktuelle Datum in einer bestimmten Zeitzone abruft:

```C#
TimeZoneInfo timeZone = TimeZoneInfo.FindSystemTimeZoneById("Central Standard Time");
DateTime now = TimeZoneInfo.ConvertTime(DateTime.Now, timeZone);
Console.WriteLine(now); //Ausgabe: 10/08/2021 10:30:00
```

# Tiefere Einblicke

Die DateTime-Klasse in C # bietet viele nützliche Methoden und Eigenschaften, um mit Datum und Uhrzeit zu arbeiten. Hier sind einige zusätzliche Informationen und Beispiele:

- Die Now-Eigenschaft gibt das aktuelle Datum und die aktuelle Uhrzeit zurück.
- Die Date-Eigenschaft gibt nur das Datum zurück, ohne die Uhrzeit.
- Die ToLocalTime() Methode kann verwendet werden, um das Datum in die lokale Zeitzone zu konvertieren.

Schauen Sie sich die offizielle Dokumentation von Microsoft an, um mehr über die DateTime-Klasse und ihre Verwendung zu erfahren.

# Siehe auch

- Offizielle Dokumentation zu DateTime in C#: https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0
- Weitere Informationen über Zeit und Datum in C#: https://www.tutorialspoint.com/csharp/csharp_date_time.htm