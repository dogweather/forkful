---
title:                "Großschreibung einer Zeichenkette"
html_title:           "Javascript: Großschreibung einer Zeichenkette"
simple_title:         "Großschreibung einer Zeichenkette"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum

Wenn du dich fragst, warum du einen String groß schreiben solltest, dann bist du hier genau richtig! In diesem Artikel werde ich dir zeigen, wie du in Javascript einen String in Großbuchstaben umwandeln kannst und warum dies nützlich sein kann.

## Wie geht's

Um einen String in Großbuchstaben zu konvertieren, gibt es in Javascript die Methode "toUpperCase()". Dies funktioniert sowohl für einzelne Wörter als auch für ganze Sätze. Schauen wir uns dazu ein Beispiel an:

```Javascript
let string = "hallo welt";
let capital = string.toUpperCase();

console.log(capital); // Ausgabe: HALLO WELT
```

Wie du sehen kannst, haben wir zuerst einen String mit dem Wert "hallo welt" erstellt. Dann haben wir die Methode "toUpperCase()" aufgerufen und das Ergebnis in der Variable "capital" gespeichert. Die Methode hat den String in Großbuchstaben umgewandelt und wir konnten ihn dann mit der "console.log()" Funktion ausgeben.

Für manche Anwendungen kann es auch nützlich sein, nur den ersten Buchstaben eines Strings zu groß zu schreiben. Dafür gibt es die Methode "charAt()" und "toUpperCase()", die zusammen verwendet werden können. Hier ist ein Beispiel:

```Javascript
let string2 = "javascript ist toll";
let firstLetter = string2.charAt(0).toUpperCase() + string2.slice(1);

console.log(firstLetter); // Ausgabe: JavaScript ist toll
```

In diesem Beispiel haben wir zuerst mit der "charAt()" Methode den ersten Buchstaben aus dem String extrahiert. Dann haben wir die "toUpperCase()" Methode aufgerufen und den Rest des Strings mit der "slice()" Methode wieder hinzugefügt.

## Ausführlichere Erklärung

Jetzt, wo du weißt, wie du einen String in Großbuchstaben umwandeln kannst, lass uns etwas tiefer in das Thema einsteigen. Der Grund, warum das Großschreiben von Strings in manchen Situationen nützlich sein kann, liegt oft darin, dass wir unseren Code einheitlicher und leichter lesbar machen möchten.

Ein gutes Beispiel dafür könnte die Validierung von Benutzereingaben sein. Wenn wir sicherstellen möchten, dass der Benutzer seinen Namen in Großbuchstaben eingeben muss, können wir den eingegebenen String einfach in Großbuchstaben umwandeln und dann vergleichen. Dadurch vermeiden wir, dass es zu Fehlern kommt, weil der Benutzer vielleicht seinen Namen mal groß und mal klein geschrieben hat.

Eine weitere Anwendungsmöglichkeit könnte darin liegen, dass wir bestimmte Wörter oder Teile eines Strings hervorheben möchten. Indem wir diese in Großbuchstaben schreiben, können wir sie optisch von dem Rest des Textes abheben.

## Siehe auch

- [String Methoden in Javascript](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String)
- [Die Wichtigkeit von einheitlichem Code](https://medium.com/better-programming/how-consistency-will-make-you-a-better-programmer-b5fb1cf3c6ae)