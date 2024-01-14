---
title:    "C#: Ein Datum in einen String umwandeln"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Warum

Das Konvertieren von einem Datum in einen String ist eine häufige Aufgabe in der Programmierung. Es kann verwendet werden, um Datumsangaben in einem einheitlichen Format anzuzeigen oder um diese in einer Datei oder einer Datenbank abzuspeichern.

## Wie man es macht

Um ein Datum in einen String umzuwandeln, gibt es in C# verschiedene Möglichkeiten. Eine davon ist die Verwendung der `ToString()`-Methode, die in der `DateTime`-Klasse enthalten ist. Diese Methode konvertiert das Datum entsprechend des angegebenen Formats.

Beispiel:

```C#
DateTime now = DateTime.Now;
string dateAsString = now.ToString("dd.MM.yyyy");
Console.WriteLine(dateAsString);
```

Output: 02.04.2021

Eine andere Möglichkeit ist die Verwendung des `String.Format()`-Befehls, der ähnlich wie die `ToString()`-Methode arbeitet. Hierbei wird jedoch ein Platzhalter für das Datum in Form von `{0}` im Format-String verwendet.

Beispiel:

```C#
DateTime now = DateTime.Now;
string dateAsString = string.Format("Heute ist der {0:d}", now);
Console.WriteLine(dateAsString);
```

Output: Heute ist der 02.04.2021

## Tiefergehende Informationen

Beim Konvertieren eines Datums in einen String ist es wichtig zu beachten, dass die Ausgabe je nach Kultur oder Region unterschiedlich sein kann. Zum Beispiel wird das Datum in den USA im Format "MM/DD/YYYY" angezeigt, während es in Deutschland im Format "DD.MM.YYYY" angezeigt wird.

Außerdem ist es möglich, benutzerdefinierte Formate zu verwenden, um das Datum in verschiedenen Stilen darzustellen. Hierzu können spezielle Formatierungssymbole wie "yyyy" für das Jahr oder "MMM" für den Monat verwendet werden.

Weitere Informationen zu den verfügbaren Formatierungssymbolen und deren Verwendung finden Sie in der offiziellen Microsoft-Dokumentation.

## Siehe auch

- [Microsoft-Dokumentation zu DateTime.ToString()](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.tostring)
- [Microsoft-Dokumentation zu String.Format()](https://docs.microsoft.com/en-us/dotnet/api/system.string.format)
- [Microsoft-Dokumentation zu benutzerdefinierten Datumsformaten](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings)