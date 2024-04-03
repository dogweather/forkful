---
date: 2024-01-20 17:32:00.602904-07:00
description: "So geht\u2019s: ."
lastmod: '2024-03-13T22:44:53.643943-06:00'
model: gpt-4-1106-preview
summary: .
title: "Berechnung eines zuk\xFCnftigen oder vergangenen Datums"
weight: 26
---

## So geht’s:
```TypeScript
const heute = new Date();
const tageZumAddieren = 10; // Zum Beispiel für eine Deadline in 10 Tagen
const zukunftigesDatum = new Date(heute.getTime() + tageZumAddieren * 24 * 60 * 60 * 1000);
console.log(zukunftigesDatum.toString()); // Output: das zukünftige Datum als String

const tageZumSubtrahieren = 5; // Für eine Erinnerung, die vor 5 Tagen fällig war
const vergangenesDatum = new Date(heute.getTime() - tageZumSubtrahieren * 24 * 60 * 60 * 1000);
console.log(vergangenesDatum.toString()); // Output: das vergangene Datum als String
```

## Deep Dive:
Früher haben Programmierer oft eigene Funktionen zur Datumsberechnung geschrieben. Diese basierten auf den Grundlagen, die bereits im ANSI C Standard definiert waren. Heute bietet JavaScript das eingebaute Date-Objekt, und TypeScript erbt diese Funktionalität. Andere Bibliotheken wie `Moment.js` oder `date-fns` erweitern diese Fähigkeiten.

Bedenkt man, dass jedes Jahr Schaltsekunden hinzukommen können und Monate unterschiedliche Längen haben, ist das Rechnen mit reinen Millisekunden allerdings nicht immer exakt. Moderne Bibliotheken berücksichtigen solche Faktoren automatisch.

Beim Arbeiten mit Zeitzonen wird es komplizierter. `Intl.DateTimeFormat` oder Bibliotheken wie `Luxon` helfen hier. TypeScript selbst bietet keine extra Date-Handling-Funktionen, da es auf JavaScript aufbaut.

## See Also:
- [MDN Web Docs - Date](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [date-fns Library](https://date-fns.org/)
- [Luxon Documentation](https://moment.github.io/luxon/#/)
- [ECMAScript 2022 Language Specification](https://tc39.es/ecma262/2022/)
