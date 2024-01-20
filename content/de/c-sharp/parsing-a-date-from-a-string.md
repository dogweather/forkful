---
title:                "Einen Datum aus einem String parsen"
html_title:           "Elixir: Einen Datum aus einem String parsen"
simple_title:         "Einen Datum aus einem String parsen"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Analysieren eines Datums aus einer Zeichenkette in C#

## Was & Warum?

Das Umwandeln eines Datums aus einer Zeichenkette, bekannt als Parsing, ist eine Methode, die verwendet wird, um eine Zeichenkette in ein Datumsformat umzuwandeln. Entwickler tun dies zum Beispiel, um im Text vorliegende Datumsformate korrekt zu verwenden oder umzuformatieren.

## So geht's:

C# bietet mit der Methode `DateTime.Parse` eine einfache und unkomplizierte Option für diese Aufgabe. Hier ist die Grundanwendung:

```C#
string dateString = "15.06.2019";
DateTime date = DateTime.Parse(dateString);
Console.WriteLine(date);  // Gibt "15.06.2019 00:00:00" aus
```

Daneben existiert noch die Methode `DateTime.TryParse`, die sicherer handhabt, falls die Formatierung des Datums nicht gültig ist:

```C#
string dateString = "nicht ein Datum";
DateTime date;
if (DateTime.TryParse(dateString, out date))
{
    Console.WriteLine(date);
}
else
{
    Console.WriteLine("Ungültiges Datumsformat!");
}  // Gibt "Ungültiges Datumsformat!" aus
```

## Tiefer eintauchen

Historisch gesehen waren bei C#'s Vorläufer Sprache C++ die Methoden fürs Parsing von Datumsformaten weit weniger benutzerfreundlich und intuitiv. 

Bei Bedarf lässt sich auch ein individuelles Format zum Parsen angeben. Hierfür stehen die Methoden `DateTime.ParseExact` und `DateTime.TryParseExact` zur Verfügung. Mit diesen Methoden können Sie ein genaues Format für die Datumsstring angeben. 

```C#
string dateString = "15 Juni 2019";
string format = "dd MMMM yyyy";
DateTime date = DateTime.ParseExact(dateString, format, CultureInfo.InvariantCulture);
Console.WriteLine(date);  // Gibt "15.06.2019 00:00:00" aus
```

Alternativen zum in C# integrierten Parsing sind z.B. Drittanbieter-Bibliotheken wie NodaTime oder DateTimeOffset, je nach Anforderungen und Vorlieben.

## Siehe auch

- Zeichenkettendarstellung eines Datums in ein DateTime-Objekt umwandeln: https://docs.microsoft.com/de-de/dotnet/api/system.datetime.parse?view=net-5.0
- Datum und Uhrzeit in .NET formatieren: https://docs.microsoft.com/de-de/dotnet/standard/base-types/formatting-dates-and-times
- Verwenden von Datum und Uhrzeit in .NET: https://docs.microsoft.com/de-de/dotnet/standard/datetime/