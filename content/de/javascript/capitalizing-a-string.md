---
title:                "String in Großbuchstaben umwandeln"
date:                  2024-01-19
simple_title:         "String in Großbuchstaben umwandeln"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Großschreiben eines Strings bedeutet, den ersten Buchstaben jedes Wortes im String in einen Großbuchstaben umzuwandeln. Programmierer verwenden diese Methode, um Texte, wie Titel oder Namen, gemäß der Grammatikregeln optisch ansprechend darzustellen.

## Anleitung:
So kannst du einen String in JavaScript großschreiben:

```javascript
function capitalizeString(str) {
  return str.replace(/\b\w/g, char => char.toUpperCase());
}

// Beispiel Nutzung
const title = "der kleine prinz";
console.log(capitalizeString(title)); // Ausgabe: "Der Kleine Prinz"
```

Eine weitere Methode, nur das erste Wort großzuschreiben:

```javascript
function capitalizeFirstWord(str) {
  return str.charAt(0).toUpperCase() + str.slice(1);
}

// Beispiel Nutzung
const book = "faust";
console.log(capitalizeFirstWord(book)); // Ausgabe: "Faust"
```

## Tiefgang:
Das Großschreiben von Strings ist historisch in der Typografie verwurzelt - es betont Wichtiges und hilft beim Verständnis des Textes. In JavaScript gab es nie eine eingebaute Methode, um diesen Prozess zu vereinfachen, also entwickelten Programmierer eigene Funktionen. Alternativen umfassen Bibliotheken wie Lodash mit `_.startCase()` für umfassendere Optionen. Die obigen Funktionen nutzen reguläre Ausdrücke bzw. String-Manipulation, um diese Aufgabe zu erfüllen. Beim Einsatz in verschiedenen Sprachen sollte auf lokale Konventionen beim Großschreiben Acht gegeben werden.

## Siehe auch:
- MDN Web Docs zum Thema String manipulation: [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String)
- Lodash Library für hilfreiche Funktionen einschließlich `_.startCase()`: [https://lodash.com/docs/#startCase](https://lodash.com/docs/#startCase)
