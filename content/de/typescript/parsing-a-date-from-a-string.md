---
title:                "Einen Datum aus einem String parsen"
html_title:           "Elixir: Einen Datum aus einem String parsen"
simple_title:         "Einen Datum aus einem String parsen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Parsen eines Datums aus einem String ist ein gebräuchlicher Weg, um Datumswerte aus Textformaten wie CSV, JSON und SQL zu extrahieren. Dies ist nützlich, da Daten häufig als Strings übertragen und anschließend in Ihrem Programm in ein praktischeres Format umgewandelt werden müssen.

## So geht's:

Ein einfacher Ansatz, um ein Datum aus einem String in TypeScript zu parsen, wäre der Einsatz des Date Konstruktors, wie im folgenden Codebeispiel gezeigt:

```TypeScript
let strDate = "2022-03-01";
let date = new Date(strDate);
console.log(date);
```
Wenn du den Code mit Node.js ausführst, erzeugt er folgende Ausgabe:

```Terminal
2022-02-28T23:00:00.000Z
```
Bitte beachte, das Datum wird in Coordinated Universal Time (UTC) ausgegeben.

## Tiefschau:

Beim Parsen eines Datums aus einem String gibt es einige wichtige Faktoren, die zu beachten sind. Während JavaScript (und in der Folge TypeScript) einen built-in Date Konstruktor zur Verfügung stellen, kann dieser zwischen verschiedenen Implementierungen variieren. Dies liegt daran, dass die ECMAScript-Spezifikation nur ein minimales Maß an Einheitlichkeit erfordert.

Es gibt auch alternative Methoden zum Parsen von Daten, wie das Modul "moment.js". Es hat eine mehr konsistente Behandlung von Datumsstrings und bietet zusätzliche Funktionen, wie die Möglichkeit, das Format des eingehenden Datumsstrings zu spezifizieren.

Falls es sich um Performance dreht, wähle die native Date-Methode in TypeScript. Sie ist eher geeignet für Anwendungen, die hohe Performance benötigen, da Dritt-Bibliotheken wie "moment.js" zusätzlichen Overhead einführen können.

## Siehe auch:

- [MDN - JavaScript Date Reference](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js Dokumentation zum Parsen von Daten](https://momentjs.com/docs/#/parsing/)
- [ECMAScript-Spezifikation](http://www.ecma-international.org/publications/standards/Ecma-262.htm)