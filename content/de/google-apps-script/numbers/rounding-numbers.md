---
title:                "Runden von Zahlen"
aliases:
- /de/google-apps-script/rounding-numbers.md
date:                  2024-02-01T22:02:45.565432-07:00
model:                 gpt-4-0125-preview
simple_title:         "Runden von Zahlen"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/google-apps-script/rounding-numbers.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Das Runden von Zahlen, ein grundlegendes Konzept in der Computerprogrammierung, beinhaltet die Anpassung einer Zahl auf die nächstgelegene ganze Zahl oder auf eine bestimmte Anzahl von Dezimalstellen. Programmierer führen das Runden oft durch, um Zahlen für die menschliche Lesbarkeit zu vereinfachen oder um spezifischen Berechnungsbedürfnissen zu entsprechen, wodurch Präzision sichergestellt und die Rechenlast reduziert wird.

## Wie:

Google Apps Script, das auf der Programmiersprache JavaScript basiert, bietet Standardmethoden zum Runden von Zahlen. Hier ist eine Aufschlüsselung von drei häufig verwendeten Techniken:

### Math.round()
Diese Funktion rundet eine Zahl auf die nächstgelegene ganze Zahl.

```javascript
var number = 2.56;
var roundedNumber = Math.round(number); 
Logger.log(roundedNumber); // Gibt aus: 3
```

### Math.ceil()
Rundet eine Zahl auf die nächstgelegene ganze Zahl auf.

```javascript
var number = 2.56;
var roundedUp = Math.ceil(number); 
Logger.log(roundedUp); // Gibt aus: 3
```

### Math.floor()
Im Gegensatz dazu, rundet eine Zahl auf die nächstgelegene ganze Zahl ab.

```javascript
var number = 2.56;
var roundedDown = Math.floor(number); 
Logger.log(roundedDown); // Gibt aus: 2
```

Für spezifische Dezimalstellen können Sie `.toFixed()` verwenden, das tatsächlich einen String zurückgibt, oder einen feineren Ansatz für mathematisches Runden:

```javascript
var number = 2.56789;
var fixedNumber = number.toFixed(2); 
Logger.log(fixedNumber); // Gibt aus: "2.57" (als String)

var preciseRound = Math.round(number * 100) / 100; 
Logger.log(preciseRound); // Gibt aus: 2.57
```

## Vertiefung

Das Runden von Zahlen in Google Apps Script weicht nicht viel von der Vorgehensweise in anderen JavaScript-Umgebungen ab. Allerdings ist das Verständnis der Unterschiede in den Rundungsmethoden und das Potenzial für Probleme mit der Fließkommazahlen-Arithmetik entscheidend. Beispielsweise können wegen der Art und Weise, wie Computer Fließkommazahlen darstellen, nicht alle Dezimalbrüche mit perfekter Genauigkeit repräsentiert werden, was manchmal zu unerwarteten Rundungsergebnissen führt.

Historisch gesehen handhabt JavaScript (und somit auch Google Apps Script) dies durch die Konformität mit dem IEEE 754-Standard, der von vielen anderen Programmiersprachen für Fließkommazahlen-Arithmetik verwendet wird. Dieser Standard definiert, wie Zahlen gerundet werden, und gewährleistet Konsistenz über verschiedene Plattformen und Sprachen hinweg.

Während direkte Rundungsmethoden in Google Apps Script unkompliziert und oft ausreichend sind, könnten komplexe oder hochpräzise Anwendungen von Bibliotheken wie decimal.js oder big.js profitieren, die für die beliebig genaue Arithmetik konzipiert sind. Diese können besonders nützlich sein, wenn Sie mit finanziellen oder wissenschaftlichen Berechnungen arbeiten, bei denen die Genauigkeit von gerundeten Zahlen von größter Bedeutung ist.

Denken Sie jedoch daran, dass die Nutzung externer Bibliotheken in Google Apps Script erfordert, dass sie durch den Skripteditor geladen werden, was Abhängigkeiten einführen oder die Leistung Ihres Skripts beeinflussen kann, je nachdem, wie es verwendet wird. In vielen Fällen sind die integrierten Mathematikmethoden vollkommen ausreichend, aber für jene Randfälle, die Präzision bis zum n-ten Grad erfordern, kann es notwendig sein, über die Standardbibliothek hinauszuschauen.
