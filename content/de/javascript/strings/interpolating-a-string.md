---
title:                "Zeichenketten interpolieren"
aliases:
- /de/javascript/interpolating-a-string/
date:                  2024-01-20T17:51:09.315513-07:00
model:                 gpt-4-1106-preview
simple_title:         "Zeichenketten interpolieren"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
String-Interpolation ermöglicht es, Variablenwerte direkt in einen String einzufügen. Sie erleichtert das Erstellen von dynamischen Strings und verbessert die Lesbarkeit des Codes.

## How to:
```javascript
// Template Literals nutzen
const name = 'Welt';
const greeting = `Hallo, ${name}!`;
console.log(greeting); // "Hallo, Welt!"

// Mit Bedingungen
const user = { name: 'Felix', isLoggedIn: true };
const welcomeMessage = `Willkommen ${user.isLoggedIn ? user.name : 'Gast'}!`;
console.log(welcomeMessage); // "Willkommen Felix!"

// Mit Funktionen
function getPreis(nettoPreis, steuer) {
  return nettoPreis * (1 + steuer);
}
const preis = getPreis(100, 0.19);
const rechnung = `Der Gesamtpreis beträgt: ${preis.toFixed(2)}€.`;
console.log(rechnung); // "Der Gesamtpreis beträgt: 119.00€."
```

## Deep Dive
Früher wurden Strings in JavaScript mit Konkatenation gebaut – durch das Verknüpfen von Variablen mit statischen Strings mittels des `+` Operators. Mit ECMAScript 2015 (auch bekannt als ES6) kamen Template Literals, welche die String-Interpolation deutlich vereinfachen. 

Statt der herkömmlichen Anführungszeichen nutzt man Backticks (\`) und fügt Variablen oder Ausdrücke mit `${}` ein. Dadurch entfallen die vielen `+` Zeichen, was den Code sauberer macht.

Alternativen wie `sprintf()` aus anderen Sprachen oder das Zusammenbauen von Strings mittels Array-Joining sind in JavaScript weniger gebräuchlich. 

Besonders praktisch wird es, wenn Funktionen oder Bedingungen in die Interpolation eingesetzt werden. So entstehen kompakte, leserliche One-Liner, die ansonsten mit mehreren Zeilen Code auskommen müssten.

## See Also
- MDN zu Template Literals: [MDN Web Docs: Template Literals](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals)
