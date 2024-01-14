---
title:    "Javascript: Berechnung eines Datums in der Zukunft oder Vergangenheit"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

# Warum

Das Berechnen von zukünftigen oder vergangenen Daten kann für verschiedene Anwendungsbereiche von Vorteil sein, wie z.B. für die Planung von Terminen, das Erstellen von Kalendern oder das Verwalten von Aufgaben. Mit Javascript können Sie ganz einfach Datumsberechnungen durchführen und so Ihre Programmierprojekte optimal unterstützen.

## Wie man es macht

Um ein Datum in der Zukunft oder Vergangenheit zu berechnen, können Sie die JavaScript-Methode `Date` verwenden. Diese Methode nimmt verschiedene Parameter wie Jahr, Monat und Tag entgegen und gibt das entsprechende Datum als Ergebnis zurück.

```Javascript
var now = new Date();
var futureDate = new Date(2022, 5, 20); // 20. Juni 2022
var pastDate = new Date(2019, 9, 10); // 10. Oktober 2019

console.log(futureDate); // Output: Fri Jun 20 2022 00:00:00 GMT+0200 (Central European Summer Time)
console.log(pastDate); // Output: Thu Oct 10 2019 00:00:00 GMT+0200 (Central European Summer Time)
```

Die Parameter werden dabei wie folgt definiert: `Date(Year, Month, Day)`. Beachten Sie dabei, dass der Monat mit 0 beginnt (0 steht für Januar, 1 für Februar usw.).

Alternativ können Sie auch die `setFullYear`, `setMonth` und `setDate` Methoden verwenden, um ein Datum zu berechnen.

```Javascript
var now = new Date();
now.setFullYear(2023); // Setzt das Jahr auf 2023
now.setMonth(9); // Setzt den Monat auf Oktober (9 entspricht dem 10. Monat)
now.setDate(28); // Setzt den Tag auf den 28.

console.log(now); // Output: Thu Oct 28 2023 14:24:31 GMT+0200 (Central European Summer Time)
```

## Tiefgreifende Informationen

Wenn Sie beispielsweise eine Applikation entwickeln, die Fälligkeitsdaten von Rechnungen berechnet, sollten Sie auch die Anzahl der Tage berücksichtigen. Hierfür können Sie die `getDate` Methode verwenden, um die Anzahl der Tage in einem Monat zu erhalten.

```Javascript
var currentDate = new Date();
var currentMonth = currentDate.getMonth(); // Gibt 9 für Oktober zurück (0 entspricht dem Januar)
var currentYear = currentDate.getFullYear(); // Gibt das aktuelle Jahr zurück

// Berechnet das nächste Monatsende
var nextDate = new Date(currentYear, currentMonth + 1, 0); // Der dritte Parameter steht für den letzten Tag des Monats
var daysInNextMonth = nextDate.getDate(); // Gibt die Anzahl der Tage im nächsten Monat zurück

console.log(daysInNextMonth); // Output: 31
```

Sie können auch die `getTime` Methode verwenden, um eine Zeitangabe in Millisekunden zu erhalten und somit die Differenz zwischen zwei Datumswerten zu berechnen.

```Javascript
var startDate = new Date(2021, 2, 15); // 15. März 2021
var endDate = new Date(2022, 7, 25); // 25. August 2022

var difference = endDate.getTime() - startDate.getTime(); // Berechnet die Differenz in Millisekunden
var daysDifference = difference / (1000 * 3600 * 24); // Konvertiert die Millisekunden in Tage

console.log(daysDifference); // Output: 528 (Anzahl der Tage zwischen beiden Daten)
```

# Siehe auch

- [Mozilla Developer Network - Date](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [W3Schools - JavaScript Dates](https://www.w3schools.com/js/js_dates.asp)
- [Date und Time in JavaScript](https://eloquentjavascript.net/09_regexp.html)