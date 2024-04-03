---
date: 2024-01-20 17:51:09.315513-07:00
description: "String-Interpolation erm\xF6glicht es, Variablenwerte direkt in einen\
  \ String einzuf\xFCgen. Sie erleichtert das Erstellen von dynamischen Strings und\
  \ verbessert\u2026"
lastmod: '2024-03-13T22:44:54.252558-06:00'
model: gpt-4-1106-preview
summary: "String-Interpolation erm\xF6glicht es, Variablenwerte direkt in einen String\
  \ einzuf\xFCgen."
title: Zeichenketten interpolieren
weight: 8
---

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
