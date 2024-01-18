---
title:                "Ein Datum aus einem String parsen"
html_title:           "C#: Ein Datum aus einem String parsen"
simple_title:         "Ein Datum aus einem String parsen"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Parsen eines Datums aus einem String ist ein häufiger Vorgang in der Programmierung, bei dem wir einen Text in ein Datumsobjekt umwandeln. Dies ist besonders nützlich, wenn wir Datumsangaben aus Benutzereingaben oder externen Datenquellen verarbeiten müssen. Durch das Parsen können wir sicherstellen, dass die Datumsinformationen korrekt interpretiert und verwendet werden.

## How to:

Um ein Datum aus einem String in C# zu parsen, verwenden wir die Methode DateTime.Parse(). Diese Methode erwartet als Argument einen String, der das Datumsformat in vordefinierter oder benutzerdefinierter Form enthält. Zum Beispiel: 

```C#
var str = "10/31/2021";
DateTime date = DateTime.Parse(str);

Console.WriteLine(date);
```
Output: 31.10.2021 00:00:00

Wir können auch eine benutzerdefinierte Kultur angeben, um sicherzustellen, dass das Datum entsprechend der regionalen Einstellungen richtig interpretiert wird. Zum Beispiel: 

```C#
var str = "10/31/2021";
CultureInfo culture = new CultureInfo("de-DE");
DateTime date = DateTime.Parse(str, culture);

Console.WriteLine(date);
```
Output: 31.10.2021 00:00:00

## Tiefer tauchen:

Das Parsen von Datumsangaben ist ein wichtiger Aspekt der Programmierung, da es uns ermöglicht, mit Datumsinformationen in verschiedenen Formaten umzugehen. Früher mussten Programmierer komplexe Algorithmen schreiben, um Datumsangaben zu verarbeiten. Mit dem Aufkommen von Programmiersprachen wie C# ist das Parsen von Datumsangaben zu einer einfachen Aufgabe geworden. Es gibt auch alternative Methoden wie TryParse(), die es uns ermöglichen, Fehler beim Parsen abzufangen und damit umzugehen.

Bei der Implementierung einer Parsing-Funktion ist es wichtig, die Eingabedaten sorgfältig zu überprüfen, um mögliche Fehler zu vermeiden. Es ist auch hilfreich, verschiedene Datumsformate zu berücksichtigen, um sicherzustellen, dass unser Code robust und flexibel ist.

## Siehe auch:

- [Microsoft Dokumentation zum Parsen von Datumsangaben in C#](https://docs.microsoft.com/de-de/dotnet/api/system.datetime.parse?view=net-6.0)
- [Codebeispiel: Datumsangabe mit benutzerdefinierter Kultur parsen](https://www.c-sharpcorner.com/UploadFile/mahesh/datetime-parsing-in-C-Sharp/)