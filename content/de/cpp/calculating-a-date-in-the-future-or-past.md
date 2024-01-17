---
title:                "Berechnung eines Datums in der Zukunft oder Vergangenheit"
html_title:           "C++: Berechnung eines Datums in der Zukunft oder Vergangenheit"
simple_title:         "Berechnung eines Datums in der Zukunft oder Vergangenheit"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Berechnen eines Datums in der Zukunft oder Vergangenheit ist eine gängige Aufgabe für Programmierer. Dabei geht es darum, ein bestimmtes Datum auf Basis eines gegebenen Datums und einer festgelegten Anzahl von Tagen in der Zukunft oder Vergangenheit zu berechnen. Diese Funktion kann in vielen verschiedenen Anwendungsbereichen nützlich sein, zum Beispiel für Terminkalender, Reiseplanung oder Finanzberechnungen.

## Wie geht das?

Um ein Datum in der Zukunft oder Vergangenheit zu berechnen, benötigt man das aktuelle Datum und eine Variable, die die Anzahl der Tage angibt, um die das Datum verschoben werden soll. Der folgende Code zeigt ein Beispiel für die Berechnung eines zukünftigen Datums, das um 10 Tage verschoben wird:

```C++
// Aktuelles Datum
int aktuellesDatum = 20210817;

// Anzahl der Tage in der Zukunft
int zukunft = 10;

// Berechnung des zukünftigen Datums
int zukuenftigesDatum = aktuellesDatum + zukunft;

// Ausgabe des Ergebnisses
cout << zukuenftigesDatum << endl;
// Ausgabe: 20210827
```

Einfach, oder? Die Berechnung eines Datums in der Vergangenheit funktioniert auf die gleiche Weise, indem man die Anzahl der Tage als negative Zahl angibt.

## Tiefere Einblicke

Die Berechnung von Daten in der Zukunft oder Vergangenheit hat eine lange Geschichte. Bereits im antiken Rom entwickelten Mathematiker komplexe Kalender, um Datumsberechnungen durchführen zu können. Heutzutage gibt es verschiedene Alternativen zur Berechnung von Datum, zum Beispiel die Verwendung von Datumsbibliotheken oder die Verwendung von eingebauten Funktionalitäten in Programmiersprachen wie PHP und JavaScript.

In der Implementierung kann es wichtig sein, auf Datums- und Zeitformate zu achten, um sicherzustellen, dass die berechneten Daten korrekt angezeigt werden. Es kann auch hilfreich sein, Randfälle wie Schaltjahre und unterschiedliche Kalenderformate zu berücksichtigen.

## Siehe auch

- [C++: Datums- und Zeitfunktionen](https://www.tutorialspoint.com/cplusplus/cpp_date_time.htm)
- [PHP: Datum und Uhrzeit](https://www.php.net/manual/de/function.date.php)
- [JavaScript: Datum und Uhrzeit](https://www.w3schools.com/js/js_dates.asp) 
- [Geschichte der Datumsberechnung](https://www.timeanddate.com/date/history.html)