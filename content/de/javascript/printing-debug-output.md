---
title:                "Javascript: Debug-Ausgabe drucken"
programming_language: "Javascript"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Warum

Debug-Ausgaben sind ein wesentlicher Bestandteil bei der Entwicklung von Javascript-Projekten. Sie helfen dabei, Fehler zu finden und den Ablauf des Codes zu verstehen. Ohne Debug-Ausgaben wäre es oft schwierig, den genauen Grund für ein unerwartetes Verhalten des Codes zu identifizieren. Deshalb ist es wichtig zu verstehen, warum und wie man Debug-Ausgaben im Javascript-Code einsetzt.

## Wie geht man vor

Um Debug-Ausgaben in Javascript zu machen, können wir die `console.log()` Funktion verwenden. Diese Funktion gibt den übergebenen Wert in der Konsole aus. Hier ist ein einfaches Beispiel:

```Javascript
let zahl = 5;
console.log(zahl); //Gibt 5 in der Konsole aus
```

Wir können auch mehrere Werte in einer Debug-Ausgabe kombinieren, indem wir sie mit Kommas trennen. Zum Beispiel:

```Javascript
let zahl1 = 5;
let zahl2 = 10;
console.log("Wert von Zahl 1:", zahl1, "Wert von Zahl 2:", zahl2); //Gibt "Wert von Zahl 1: 5 Wert von Zahl 2: 10" in der Konsole aus
```

Wir können auch die `console.log()` Funktion in Bedingungen oder Schleifen verwenden, um den Ablauf des Codes zu verfolgen. Hier ist ein Beispiel, das zählt, wie oft eine Schleife durchlaufen wird:

```Javascript
for (let i = 0; i < 10; i++) {
  console.log("Schleifendurchlauf:", i+1); //Gibt "Schleifendurchlauf: 1, Schleifendurchlauf: 2" usw. bis "Schleifendurchlauf: 10" in der Konsole aus
}
```

## Tiefes Eintauchen

Um Debug-Ausgaben effektiv zu nutzen, ist es wichtig zu verstehen, wann und wo sie platziert werden sollten. Es ist sinnvoll, Debug-Ausgaben in Teilen des Codes zu platzieren, wo es zu Problemen kommen könnte oder um bestimmte Variablen zu verfolgen. Dabei ist es jedoch wichtig, am Ende des Debugging- Prozesses alle Debug-Ausgaben zu entfernen, um eine unnötige Belastung des Codes zu vermeiden.

Außerdem ist es hilfreich, die verschiedenen Methoden der `console`-Objekts zu kennen, wie z.B. `console.warn()` für Warnungen und `console.error()` für Fehlermeldungen. Diese können dazu beitragen, den Code besser zu strukturieren und Probleme schnell zu identifizieren.

## Siehe auch
- [Javascript Debugging Tutorial](https://www.w3schools.com/js/js_debugging.asp)
- [Console Object in Javascript](https://developer.mozilla.org/en-US/docs/Web/API/Console)
- [Why is Debugging Important?](https://www.geeksforgeeks.org/why-debugging-is-important-in-programming/)