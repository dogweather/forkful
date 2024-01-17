---
title:                "Das aktuelle Datum abrufen"
html_title:           "C#: Das aktuelle Datum abrufen"
simple_title:         "Das aktuelle Datum abrufen"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Was & Warum?
Die aktuelle Version von C# bietet Programmierern eine einfache Möglichkeit, das aktuelle Datum in ihrer Anwendung abzurufen. Das aktuelle Datum ist einfach das Datum, das in diesem Moment gilt, unabhängig von der Zeitzone oder dem geografischen Standort des Benutzers. Programmierer nutzen diese Funktion, um aktuelle Informationen in ihre Anwendung einzubinden oder um bestimmte Aufgaben basierend auf dem aktuellen Datum auszuführen.

## Wie geht das?
Um das aktuelle Datum in C# abzurufen, können wir die `DateTime`-Klasse verwenden, die Teil des .NET Frameworks ist. Hier ist ein Beispielcode, der das aktuelle Datum und die aktuelle Uhrzeit in der Konsole ausgibt:

```C#
DateTime now = DateTime.Now;
Console.WriteLine("Das aktuelle Datum ist: " + now.ToString("dd/MM/yyyy"));
Console.WriteLine("Die aktuelle Uhrzeit ist: " + now.ToString("HH:mm:ss"));
```

Die Ausgabe dieses Codes wäre etwas wie: "Das aktuelle Datum ist: 19/09/2021" und "Die aktuelle Uhrzeit ist: 14:30:00".

## Tiefes Tauchen
Das Konzept des aktuellen Datums ist nicht neu und hat eine lange Geschichte in der Programmierung. Frühere Sprachen wie Visual Basic haben ihre eigenen Methoden zur Abfrage des aktuellen Datums gehabt, aber mit C# können wir dies jetzt einheitlich mit der `DateTime`-Klasse tun.

Natürlich gibt es auch alternative Möglichkeiten, um das aktuelle Datum in einer Anwendung zu nutzen. Zum Beispiel können wir die LocalTime-Funktion verwenden, um das aktuelle Datum für eine bestimmte Zeitzone abzurufen. Wir können auch die `DateTime`-Struktur verwenden, um komplexe Berechnungen mit Datum und Uhrzeit durchzuführen.

## Siehe auch
Weitere Informationen zum Abrufen des aktuellen Datums in C# finden Sie in der offiziellen Dokumentation von Microsoft: https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0

Für weitere praktische Anleitungen und Tipps zur Verwendung von C# besuchen Sie gerne unsere Webseite: https://www.c-sharpcorner.com/