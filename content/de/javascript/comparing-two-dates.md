---
title:    "Javascript: Vergleich zweier Daten"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Warum
Das Vergleichen von zwei Daten kann für Programmierer*innen sehr nützlich sein, um zu überprüfen, ob bestimmte Ereignisse in der richtigen Reihenfolge stattgefunden haben oder um Daten in einer bestimmten Zeitspanne auszuwählen.

## Wie geht man vor
Um zwei Daten in Javascript zu vergleichen, kann man den Dateien einen numerischen Wert zuweisen und diese dann mit logischen Operatoren vergleichen. Zum Beispiel: 

```Javascript
const date1 = new Date("2021-05-20");
const date2 = new Date("2021-05-22");

if (date1 < date2){
  console.log("Date 1 liegt vor Date 2");
}
```

Das gibt die Ausgabe "Date 1 liegt vor Date 2" aus, da der numerische Wert von Date 1 kleiner als der von Date 2 ist. 
Man kann auch die `getTime()` Methode verwenden, um den numerischen Wert jeder Datei zu bekommen und diese dann zu vergleichen. 

```Javascript
if (date1.getTime() < date2.getTime()){
  console.log("Date 1 liegt vor Date 2");
}
```

Es ist auch möglich, zwei Daten direkt miteinander zu vergleichen, ohne ihnen einen numerischen Wert zuzuweisen. In diesem Fall werden sie automatisch in numerische Werte umgewandelt und verglichen. 

## Tiefergehende Information
Beim Vergleichen von zwei Daten gibt es einige wichtige Dinge zu beachten. Zum Beispiel kann es Unterschiede in der Zeitzone geben, die berücksichtigt werden müssen. Auch die Datumsformate können variieren und müssen daher möglicherweise angepasst werden, damit sie miteinander vergleichbar sind. 

Es ist auch wichtig zu wissen, dass das Vergleichen von zwei Daten mithilfe von logischen Operatoren darauf basiert, welches Datum einen größeren numerischen Wert hat. Wenn die beiden Daten gleich sind, wird der Ausdruck als "false" ausgewertet. 

## Siehe auch
- [MDN - Vergleichen von Daten in Javascript](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Operators/Dates)
- [Stack Overflow - How to compare two dates in Javascript](https://stackoverflow.com/questions/492994/compare-two-dates-with-javascript)
- [W3Schools - Javascript Dates](https://www.w3schools.com/js/js_dates.asp)