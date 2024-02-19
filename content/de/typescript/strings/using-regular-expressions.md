---
aliases:
- /de/typescript/using-regular-expressions/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:30.997038-07:00
description: "Regul\xE4re Ausdr\xFCcke, oder Regex, sind ein m\xE4chtiges Werkzeug\
  \ f\xFCr Mustererkennung und -suche in der Programmierung. Programmierer nutzen\
  \ Regex f\xFCr Aufgaben\u2026"
lastmod: 2024-02-18 23:09:04.586037
model: gpt-4-0125-preview
summary: "Regul\xE4re Ausdr\xFCcke, oder Regex, sind ein m\xE4chtiges Werkzeug f\xFC\
  r Mustererkennung und -suche in der Programmierung. Programmierer nutzen Regex f\xFC\
  r Aufgaben\u2026"
title: "Regul\xE4re Ausdr\xFCcke verwenden"
---

{{< edit_this_page >}}

## Was & Warum?
Reguläre Ausdrücke, oder Regex, sind ein mächtiges Werkzeug für Mustererkennung und -suche in der Programmierung. Programmierer nutzen Regex für Aufgaben wie die Validierung von Benutzereingaben, das Durchsuchen von Texten oder das Manipulieren von Zeichenketten, weil es effizient und vielseitig ist.

## Wie geht das:

Lassen Sie uns in TypeScript eintauchen und sehen, wie Regex für gängige Aufgaben verwendet wird.

```TypeScript
// Definiere ein Regex-Muster für eine E-Mail-Adresse
const emailPattern = /\S+@\S+\.\S+/;

// Teste, ob eine Zeichenkette dem E-Mail-Muster entspricht
const email = "user@example.com";
console.log(emailPattern.test(email)); // Ausgabe: true

// Finde und ersetze Ziffern in einer Zeichenkette
const replaceDigits = "Artikel 25 kostet $30".replace(/\d+/g, '#');
console.log(replaceDigits); // Ausgabe: "Artikel # kostet $#"

// Extrahieren spezifischer Teile aus einer Zeichenkette mit Hilfe von Erfassungsgruppen
const data = "April 10, 2021";
const datePattern = /(\w+) (\d+), (\d+)/;
const [, month, day, year] = datePattern.exec(data) || [];
console.log(month, day, year); // Ausgabe: "April" "10" "2021"
```

## Vertiefung

Bereits in den 1950ern beschrieb der Mathematiker Stephen Kleene reguläre Ausdrücke als ein Modell zur Darstellung regulärer Sprachen, was später in der Informatik unerlässlich wurde. Heute ist Regex in der Programmierung für den Umgang mit Text allgegenwärtig.

Während Regex ein Schweizer Taschenmesser für Zeichenkettenoperationen ist, gibt es Alternativen. Abhängig von der Komplexität der Aufgabe können manchmal Zeichenkettenmethoden wie `includes()`, `startsWith()`, `endsWith()` oder sogar das Parsen mit einer Bibliothek besser sein. Zum Beispiel kann das Parsen einer komplexen JSON-Zeichenkette mit Regex zum Alptraum werden – verwenden Sie stattdessen einen JSON-Parser.

Bezüglich der Implementierung basieren Regex in JavaScript und TypeScript auf der ECMAScript-Sprachspezifikation. Unter der Haube verwenden Engines Zustandsmaschinen, um Muster effizient abzugleichen. Es ist erwähnenswert, dass Regex-Operationen in Bezug auf die Leistung teuer werden können, insbesondere bei schlecht geschriebenen Mustern – achten Sie auf "katastrophales Backtracking".

## Siehe auch

- MDN Web Docs über reguläre Ausdrücke: [MDN Reguläre Ausdrücke](https://developer.mozilla.org/de/docs/Web/JavaScript/Guide/Regular_Expressions)
- Regex101: Ein Werkzeug zum Testen und Debuggen von Regex-Mustern [Regex101](https://regex101.com/)
- Buch "Mastering Regular Expressions" für tiefgehendes Verständnis: [O'Reilly](https://www.oreilly.com/library/view/mastering-regular-expressions/0596528124/)
