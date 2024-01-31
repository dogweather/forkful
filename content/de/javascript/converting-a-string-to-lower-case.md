---
title:                "Umformung eines Strings in Kleinbuchstaben"
date:                  2024-01-20T17:38:43.396336-07:00
model:                 gpt-4-1106-preview
simple_title:         "Umformung eines Strings in Kleinbuchstaben"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? (Was & Warum?)
Ein String in Kleinbuchstaben umzuwandeln, bedeutet, jeden Großbuchstaben in seinem Äquivalent in Kleinbuchstaben zu ändern. Programmierer nutzen das, um die Nutzereingaben zu vereinheitlichen oder um Textvergleiche durchzuführen, unabhängig von der Groß- oder Kleinschreibung.

## How to: (Wie geht das?)
In JavaScript wandelt die Funktion `toLowerCase()` eines String-Objekts den gesamten Text in Kleinbuchstaben um. Hier ist es in Aktion:

```javascript
let greeting = "Hallo Welt!";
let lowerCaseGreeting = greeting.toLowerCase();

console.log(lowerCaseGreeting); // "hallo welt!"
```

Ganz einfach, richtig? Der Output ist ein kleingeschriebener Text, egal, wie der ursprüngliche String aussieht.

## Deep Dive (Tiefer eintauchen)
Die Funktion `toLowerCase()` gibt es schon eine Weile und spielt seit langem eine wichtige Rolle bei Textverarbeitung und -vergleich. Historisch gesehen verhindert diese Funktionalität Probleme mit der Groß- und Kleinschreibung und ist besonders nützlich bei der Suchfunktionalität in Datenbanken und Webanwendungen.

### Alternativen
Es gibt auch die Funktion `toLocaleLowerCase()`, die ähnlich wirkt, aber Besonderheiten bezüglich der länderspezifischen Buchstaben berücksichtigt – zum Beispiel das deutsche "ß":

```javascript
let germanText = "Straße";
console.log(germanText.toLowerCase()); // "straße"
console.log(germanText.toLocaleLowerCase('de-DE')); // "straße"
```

### Implementierungsdetails
`toLowerCase()` durchläuft jeden Buchstaben im String und prüft dessen Unicode-Wert. Wenn der Buchstabe ein Großbuchstabe ist, wird er basierend auf den Unicode-Regeln in den entsprechenden Kleinbuchstaben umgewandelt.

## See Also (Siehe auch)
- MDN Web Docs für `toLowerCase()`: https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase
- Unicode-Standard: https://unicode.org/
- JavaScript Equality Table für Stringvergleiche: https://dorey.github.io/JavaScript-Equality-Table/
