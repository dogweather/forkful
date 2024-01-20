---
title:                "Vergleich von zwei Daten"
html_title:           "C#: Vergleich von zwei Daten"
simple_title:         "Vergleich von zwei Daten"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Vergleich von zwei Daten in Javascript

## Was & Warum? 
Datenvergleich ist der Vorgang, bei dem zwei Datumswerte miteinander verglichen werden, um zu überprüfen, welches Datum früher oder später im Kalender liegt. Programmierer verwenden diese Methode häufig bei der Erstellung von zeitabhängigen Algorithmen oder beim Sortieren von Datumslisten.

## Wie zu:
Die `Date`-Objekttyp von JavaScript ermöglicht es uns, mit Daten zu arbeiten. Hier sind einige Beispiele, wie man zwei Daten im JavaScript vergleichen kann.

```Javascript
let date1 = new Date(2019, 11, 24);
let date2 = new Date(2020, 11, 24);

// Überprüfen Sie, ob date1 früher als date2 ist
if(date1 < date2) {
  console.log("date1 ist früher");
} else {
  console.log("date2 ist früher");
}
```
Ergebnisausgabe:
```Javascript
"date1 ist früher"
```

## Tiefen Tauchgang
Historisch gesehen wurde die `getTime()`-Methode im JavaScript verwendet, um die Millisekunden seit dem 1. Januar 1970 00:00:00 UTC bis zum angegebenen Datum zurückzugeben. Dies ermöglichte einen numerischen Vergleich von zwei Daten. 

```JavaScript
if(date1.getTime() < date2.getTime()) {
  console.log("date1 ist früher");
} else {
  console.log("date2 ist früher");
}
```
Während die `getTime()`- Methode immer noch gültig ist, ermöglicht die direkte Vergleichsoperation eine saubere und kompakte Alternative. 

Die direkte Vergleichsoperation verwendet intern `valueOf()`, die das gleiche Ergebnis wie `getTime()` zurückgibt. Aus diesem Grund kann das `Date`-Objekt direkt in Vergleichsoperationen verwendet werden.

## Siehe auch
Für weitere Informationen können Sie die folgenden Ressourcen besuchen:
- [MDN Web Docs: Date](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [ECMAScript® 2020 Language Specification: Date Objects](https://tc39.es/ecma262/#sec-date-objects)