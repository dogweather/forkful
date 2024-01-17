---
title:                "Das Berechnen eines Datums in der Zukunft oder Vergangenheit"
html_title:           "PowerShell: Das Berechnen eines Datums in der Zukunft oder Vergangenheit"
simple_title:         "Das Berechnen eines Datums in der Zukunft oder Vergangenheit"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Was & Warum? 
Die Berechnung eines Datums in der Zukunft oder Vergangenheit ist ein häufiges Problem bei der Programmierung. Programme müssen oft Daten berechnen, die entweder in der Vergangenheit oder Zukunft liegen. Dies kann zum Beispiel für die Planung von Terminen oder die Verarbeitung von Daten aus verschiedenen Zeitzonen notwendig sein.

## Wie geht's:
Um ein Datum in der Zukunft oder Vergangenheit zu berechnen, kann man die eingebaute PowerShell-Funktion "Get-Date" verwenden. Die Funktion akzeptiert einen Parameter "-Date", der entweder ein Datum oder eine Zeichenfolge sein kann. Folgendes Beispiel berechnet das Datum, das 10 Tage in der Zukunft liegt:

```PowerShell
$futureDate = (Get-Date).AddDays(10)
echo $futureDate
```

Die Ausgabe dieses Codes wäre das Datum, das 10 Tage in der Zukunft liegt, und würde in folgendem Format angezeigt werden: "mm/dd/yyyy hh:mm:ss".

Um ein Datum in der Vergangenheit zu berechnen, kann man die gleiche Funktion verwenden, jedoch mit einem negativen Wert für "AddDays". Zum Beispiel würde dieser Code das Datum, das 10 Tage in der Vergangenheit liegt, berechnen:

```PowerShell
$pastDate = (Get-Date).AddDays(-10)
echo $pastDate
```

## Tiefenschärfe:
Die Berechnung von Datum und Zeit hat eine lange Geschichte. Bereits im alten Ägypten und Mesopotamien wurden Methoden zur Berechnung von Kalendern entwickelt. Heute gibt es viele verschiedene Arten von Kalendern, wie den gregorianischen Kalender, den julianischen Kalender und den chinesischen Kalender.

Eine alternative Methode zur Berechnung von Datumsangaben ist die Verwendung der .NET-Klasse "DateTime". Diese Klasse bietet ähnliche Funktionen zur Arbeit mit Datums- und Zeitangaben wie die "Get-Date" Funktion, aber mit zusätzlichen Optionen und Flexibilität.

## Siehe auch:
- [Microsoft Dokumentation zu "Get-Date" Funktion](https://docs.microsoft.com/de-de/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7)
- [Weitere Informationen über Kalender und Zeit](https://de.wikipedia.org/wiki/Kalender)
- [PowerShell-Dokumentation zu DateTime-Klasse](https://docs.microsoft.com/de-de/dotnet/api/system.datetime?view=netcore-3.1)