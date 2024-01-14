---
title:    "Javascript: Vergleich von zwei Daten"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

##Warum
Das Vergleichen von zwei Daten kann hilfreich sein, um festzustellen, ob ein Datum vor oder nach dem anderen liegt. Dies ist besonders nützlich bei der Sortierung von Daten oder beim Erstellen von Bedingungen in einer Anwendung.

##Wie geht es
Das Vergleichen von zwei Daten in Javascript ist relativ einfach. Zuerst müssen Sie beide Daten in das Date-Objekt umwandeln, wenn sie nicht bereits in diesem Format vorliegen. Dann können Sie den Vergleichsoperator (`<`, `>`, `<=`, `>=`) verwenden, um die Beziehung zwischen den beiden Daten zu überprüfen. Hier ist ein Beispielcode, der zwei Daten vergleicht und das Ergebnis in der Konsole ausgibt:

```Javascript
const date1 = new Date('2021-01-01');
const date2 = new Date('2021-02-01');

if (date1 < date2) {
  console.log('date1 liegt vor date2');
} else if (date1 > date2) {
  console.log('date1 liegt nach date2');
} else {
  console.log('Beide Daten sind gleich');
}
```

Die Ausgabe dieses Codes wäre `date1 liegt vor date2`, da der 1. Januar 2021 vor dem 1. Februar 2021 liegt. Beachten Sie, dass bei der Verwendung der `<=` und `>=` Operatoren das Gleichheitszeichen berücksichtigt wird.

##Tiefer tauchen
Beim Vergleichen von zwei Daten gibt es einige Dinge zu beachten. Zum Beispiel kann es zu unerwarteten Ergebnissen führen, wenn Sie Daten mit unterschiedlichen Zeitzonen vergleichen. Auch die Zeitkomponente eines Datums muss berücksichtigt werden. Wenn Sie also zwei Daten mit der gleichen Datumsangabe, aber unterschiedlichen Uhrzeiten vergleichen, werden sie nicht als gleich betrachtet.

Ein guter Tipp ist es, bei der Arbeit mit Daten immer einheitliche Vergleichsstandards zu verwenden, z.B. die Datumsangabe in UTC Zeit oder die Verwendung von Datumsfunktionen, die Zeitunterschiede berücksichtigen. Es ist auch wichtig zu beachten, dass durch die Umwandlung von Daten in das Date-Objekt bestimmte Genauigkeitsverluste auftreten können, so dass Vergleiche zu Fehlern führen können. Es ist daher ratsam, die Zeitkomponente in einem Datumsvergleich zu berücksichtigen.

##Siehe auch
- [MDN Web Docs - Date objects](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [W3Schools - Compare Dates](https://www.w3schools.com/js/js_dates_compare.asp)
- [Stack Overflow - How to compare two dates in Javascript](https://stackoverflow.com/questions/492994/compare-two-dates-with-javascript)