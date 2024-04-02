---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:36.988553-07:00
description: "Das Kapitalisieren eines Strings bedeutet, das erste Zeichen des Strings\
  \ in einen Gro\xDFbuchstaben umzuwandeln, w\xE4hrend die restlichen Zeichen unver\xE4\
  ndert\u2026"
lastmod: '2024-03-13T22:44:54.249771-06:00'
model: gpt-4-0125-preview
summary: "Das Kapitalisieren eines Strings bedeutet, das erste Zeichen des Strings\
  \ in einen Gro\xDFbuchstaben umzuwandeln, w\xE4hrend die restlichen Zeichen unver\xE4\
  ndert\u2026"
title: "Einen String gro\xDFschreiben"
weight: 2
---

## Was & Warum?
Das Kapitalisieren eines Strings bedeutet, das erste Zeichen des Strings in einen Großbuchstaben umzuwandeln, während die restlichen Zeichen unverändert bleiben. Diese Operation wird in JavaScript häufig durchgeführt, um Benutzereingaben zu formatieren, Namen oder Titel anzuzeigen und Konsistenz in den Texten der Benutzeroberfläche zu gewährleisten.

## Wie geht das:
In JavaScript gibt es keine integrierte Methode, um Strings direkt zu kapitalisieren, aber es ist einfach, dies mit grundlegenden String-Manipulationsmethoden zu implementieren.

### Mit Standard JavaScript
```javascript
function capitalize(str) {
  if (!str) return '';
  return str.charAt(0).toUpperCase() + str.slice(1);
}

console.log(capitalize('hello world')); // Ausgabe: "Hello world"
```

### ES6-Version
Mit ES6-Template-Literalen kann die Funktion auf eine prägnantere Weise geschrieben werden:
```javascript
const capitalize = (str) => !str ? '' : `${str[0].toUpperCase()}${str.slice(1)}`;

console.log(capitalize('hello ES6')); // Ausgabe: "Hello ES6"
```

### Mit Lodash
Lodash ist eine beliebte Drittanbieter-Utility-Bibliothek, die eine breite Palette von Funktionen bietet, um mit JavaScript-Werten zu arbeiten und diese zu manipulieren, einschließlich Strings. Um einen String mit Lodash zu kapitalisieren:
```javascript
// Zuerst, falls noch nicht geschehen, Lodash installieren: npm install lodash
const _ = require('lodash');

console.log(_.capitalize('LODASH Beispiel')); // Ausgabe: "Lodash beispiel"
```
_Beachten Sie, wie Lodash nicht nur den ersten Buchstaben kapitalisiert, sondern auch den Rest des Strings in Kleinbuchstaben umwandelt, was sich leicht von der einfachen JavaScript-Implementierung unterscheidet._

### Mit CSS (Nur für Anzeigezwecke)
Wenn das Ziel ist, Text für die Anzeige in der UI zu kapitalisieren, kann CSS verwendet werden:
```css
.capitalize {
  text-transform: capitalize;
}
```
```html
<div class="capitalize">hello css</div> <!-- Wird angezeigt als "Hello css" -->
```
**Hinweis:** Diese Methode ändert, wie der Text auf der Webseite erscheint, ohne den String selbst in JavaScript zu verändern.
