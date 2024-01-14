---
title:                "C#: Umwandeln von einem Datum in einen String."
programming_language: "C#"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Konvertieren von Daten in einen String ist ein häufiger Schritt beim Programmieren. Es kann hilfreich sein, um Daten besser lesbar oder manipulierbar zu machen. In diesem Blogbeitrag werden wir uns genauer anschauen, wie man in C# ein Datum in einen String umwandelt.

## Wie geht das?

Um ein Datum in einen String umzuwandeln, gibt es verschiedene Methoden in C#. Eine davon ist die Verwendung der `ToString()`-Methode, die in das `DateTime`-Objekt eingebaut ist. Diese Methode ermöglicht es uns, das Datum in verschiedenen Formaten darzustellen, je nach Bedarf. Hier ist ein Beispielcode, der ein Datum in den String "dd.MM.yyyy" umwandelt:

```C#

DateTime now = DateTime.Now;
string dateAsString = now.ToString("dd.MM.yyyy");
Console.WriteLine(dateAsString);

```

Ausgabe:

```
05.10.2021
```

Wir können auch andere Formate wie "MM/dd/yyyy" oder "yyyy-MM-dd" verwenden, je nachdem, welches Format in unserer Anwendung benötigt wird. Hier ist ein Beispiel, wie wir das Format "MM/dd/yyyy" verwenden können:

```C#

DateTime now = DateTime.Now;
string dateAsString = now.ToString("MM/dd/yyyy");
Console.WriteLine(dateAsString);

```

Ausgabe:

```
10/05/2021
```

Es ist auch möglich, zusätzliche Informationen hinzuzufügen, wie z.B. die Uhrzeit. Dazu können wir das Format "dd.MM.yyyy HH:mm:ss" verwenden:

```C#

DateTime now = DateTime.Now;
string dateAsString = now.ToString("dd.MM.yyyy HH:mm:ss");
Console.WriteLine(dateAsString);

```

Ausgabe:

```
05.10.2021 15:54:42
```

Wir können auch das Kürzen und Auffüllen von Werten steuern, indem wir das Format "`d`" oder "`D`" verwenden. Hier ist ein Beispiel, wie wir die Monatsbezeichnungen auf 3 Buchstaben kürzen können:

```C#

DateTime now = DateTime.Now;
string dateAsString = now.ToString("dd.MMM.yyyy");
Console.WriteLine(dateAsString);

```

Ausgabe:

```
05.Okt.2021
```

Weitere Informationen zu den verfügbaren Formatierungsoptionen finden Sie in der offiziellen C#-Dokumentation.

## Tiefergehende Informationen

Beim Konvertieren von Daten in einen String gibt es einige wichtige Dinge zu beachten. Zum Beispiel kann es zu Fehlern kommen, wenn die verwendeten Formate nicht richtig interpretiert werden können. Es ist auch wichtig, sich über unterschiedliche lokale Datumsformate bewusst zu sein, da diese je nach Region variieren können.

Darüber hinaus gibt es auch Möglichkeiten, benutzerdefinierte Formate zu erstellen und zu verwenden. Wenn Sie tiefer in das Thema einsteigen möchten, empfehlen wir Ihnen, sich mit der `DateTimeFormatInfo`-Klasse und ihren Eigenschaften und Methoden vertraut zu machen.

## Siehe auch

- [C#-DateTime.ToString() Methode](https://docs.microsoft.com/de-de/dotnet/api/system.datetime.tostring?view=net-5.0)
- [DateTimeFormatInfo-Klasse](https://docs.microsoft.com/de-de/dotnet/api/system.globalization.datetimeformatinfo?view=net-5.0)