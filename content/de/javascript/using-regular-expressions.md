---
title:                "Einsatz von regulären Ausdrücken"
date:                  2024-01-19
simple_title:         "Einsatz von regulären Ausdrücken"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Was & Warum?
Reguläre Ausdrücke (Regex) sind Muster, mit denen du in Strings nach bestimmten Textpassagen suchst und arbeitest. Programmierer verwenden sie wegen ihrer Effizienz bei der Textverarbeitung und Automatisierung wie Datenvalidierung und -manipulation.

## So geht's:

```Javascript
// Einfache Suche nach "Hallo"
let text = "Hallo Welt!";
let regex = /Hallo/;
console.log(regex.test(text)); // Output: true

// Ersetzen von "Welt" durch "Javascript"
text = text.replace(/Welt/, "Javascript");
console.log(text); // Output: "Hallo Javascript!"

// Überprüfung einer E-Mail-Adresse
let email = "info@example.com";
let emailRegex = /\S+@\S+\.\S+/;
console.log(emailRegex.test(email)); // Output: true
```

## Vertiefung:
Reguläre Ausdrücke wurden in den 1950er Jahren von Stephen Kleene erfunden und sind heute in fast allen Programmiersprachen verfügbar. Alternativen wie String-Funktionen (`indexOf`, `startsWith` usw.) sind oft weniger mächtig und flexibel. Die Implementierung von Regex variiert zwischen Sprachen, aber die zugrundeliegende Theorie der formalen Sprachen ist universell.

## Siehe auch:
- MDN Web Docs zu regulären Ausdrücken: [MDN Regular Expressions](https://developer.mozilla.org/de/docs/Web/JavaScript/Guide/Regular_Expressions)
- Regex-Tester und -Debugger: [regex101](https://regex101.com/)
