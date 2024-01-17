---
title:                "Umwandeln eines Datums in einen String"
html_title:           "C#: Umwandeln eines Datums in einen String"
simple_title:         "Umwandeln eines Datums in einen String"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Wenn wir als Programmierer mit Datumsangaben arbeiten, müssen wir manchmal diese Daten in einen String umwandeln. Das bedeutet, dass wir das Datum in eine lesbare Zeichenkette verwandeln, die wir dann beispielsweise in einer Benutzeroberfläche anzeigen können. Wir müssen dies tun, weil Computer in der Regel Datumsangaben in einem maschinellen Format speichern, das wir Menschen nicht gut lesen können. 

## Wie geht es?

Um ein Datum in einen String umzuwandeln, können wir in C# die Methode `ToString()` benutzen. Hier ein Beispiel:

```C#
DateTime now = DateTime.Now; // current date and time
string dateString = now.ToString(); // dateString is now "9/23/2021 10:21:00 AM"
```

Wir können auch ein bestimmtes Format für den String angeben, indem wir dem `ToString()` Aufruf einen Parameter übergeben:

```C#
DateTime now = DateTime.Now; // current date and time
string dateString = now.ToString("dd MMMM yyyy"); // dateString is now "23 September 2021"
```

Wir können verschiedene String-Formate ausprobieren und sehen, welches am besten zu unseren Anforderungen passt. Hier sind einige gängige Formate:

- "dd/MM/yyyy" - 23/09/2021
- "MM/dd/yyyy" - 09/23/2021
- "dd MMMM yyyy" - 23 September 2021
- "yyyy/MM/dd" - 2021/09/23

## Tiefer Einblick

Die Umwandlung eines Datums in einen String hat eine lange Geschichte in der Programmierung. In früheren Programmiersprachen war dies eine komplexe Aufgabe, aber dank moderner Sprachen wie C# ist es jetzt viel einfacher geworden. Es gibt auch alternative Methoden, um Datums-Strings zu formatieren, wie zum Beispiel die`DateTime.ParseExact()`-Methode.

Bei der Implementierung der `ToString()` Methode wird das verwendete Format durch die Systemeinstellungen des Computers bestimmt. Dies bedeutet, dass der gleiche Code auf unterschiedlichen Computern möglicherweise unterschiedliche Ergebnisse liefert. Um dies zu vermeiden, können wir das Format explizit angeben, wie wir im Beispiel oben gesehen haben.

## Siehe auch

- [Offizielle Dokumentation von Microsoft zu `ToString()`](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.tostring?view=net-5.0)
- [C#-Beispiele für die Verwendung von `ToString()`](https://www.c-sharpcorner.com/blogs/how-to-convert-a-datetime-to-string-format-in-c-sharp-programming1)