---
title:    "Javascript: Lesen von Befehlszeilenargumenten"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Warum

Das Lesen von Befehlszeilenargumenten ist ein grundlegendes Konzept für alle, die in der Javascript-Programmierung tätig sind. Es ermöglicht es einem Benutzer, Eingaben direkt über die Befehlszeile an das Programm zu übergeben und somit die Flexibilität und Effizienz des Programms zu steigern.

## Wie es geht

Der erste Schritt zum Lesen von Befehlszeilenargumenten in Javascript ist das Importieren von "process.argv". Dies ist ein vordefinierter Array, der alle übergebenen Argumente enthält. Im Folgenden ist ein Beispielcode zum Lesen von zwei übergebenen Argumenten:

```Javascript
const args = process.argv.slice(2); // Die ersten beiden Elemente in "process.argv" sind Standardargumente
console.log("Erstes Argument:", args[0]); // Ausgabe des ersten Arguments
console.log("Zweites Argument:", args[1]); // Ausgabe des zweiten Arguments
```

Angenommen, das obige Skript wird mit dem Befehl "node readArguments.js David 27" ausgeführt, wäre die Ausgabe wie folgt:

```
Erstes Argument: David
Zweites Argument: 27
```

## Tiefer Einblick

Beim Lesen von Befehlszeilenargumenten gibt es noch einige wichtige Punkte zu beachten. Zunächst werden alle übergebenen Werte als Strings interpretiert. Wenn also ein numerisches Argument benötigt wird, muss es explizit in das gewünschte Datentyp konvertiert werden. Auch ist zu beachten, dass das Lesen von Argumenten zu Beginn des Programms erfolgt, daher müssen Änderungen an "process.argv" direkt am Anfang vorgenommen werden.

## Siehe auch

- [Offizielle Dokumentation zu Process-Objekten](https://nodejs.org/api/process.html)
- [Tutorial zum Lesen von Befehlszeilenargumenten in Javascript](https://www.digitalocean.com/community/tutorials/how-to-read-command-line-arguments-in-node-js)