---
title:                "Zahlen runden"
date:                  2024-01-26T03:45:09.969000-07:00
model:                 gpt-4-0125-preview
simple_title:         "Zahlen runden"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/rounding-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?
Runden ist das Abschneiden von „Rauschen“ an einer Zahl nach einem bestimmten Punkt. Programmierer runden, um die Genauigkeit zu kontrollieren, Speicher zu verwalten oder die Ausgabe benutzerfreundlich zu machen – wie zum Beispiel 2,998 in eine saubere 3 umzuwandeln.

## Wie geht das:
So runden Sie Zahlen in JavaScript unter Verwendung von `Math.round()`, `Math.ceil()` und `Math.floor()`: 

```javascript
let originalNumber = 2.567;

let roundedDown = Math.floor(originalNumber); // 2
let roundedUp = Math.ceil(originalNumber);    // 3
let rounded = Math.round(originalNumber);     // 3 (da .567 mehr als .5 ist)

console.log(roundedDown); // Gibt aus: 2
console.log(roundedUp);   // Gibt aus: 3
console.log(rounded);     // Gibt aus: 3
```

Um auf eine bestimmte Anzahl von Dezimalstellen zu runden, verwenden Sie `toFixed()`:

```javascript
let twoDecimals = originalNumber.toFixed(2); // "2.57" (gibt einen String zurück)

console.log(twoDecimals); // Gibt aus: "2.57"
```

Konvertieren Sie den String mit einem unären Plus oder `Number()` zurück in eine Zahl:

```javascript
let numberAgain = +twoDecimals; // 2.57

console.log(numberAgain); // Gibt aus: 2.57
```

## Vertiefung
Zahlen zu runden ist nichts Neues; es ist so alt wie die Zahlen selbst. In JavaScript verwendet `Math.round()` das „Runden zur nächsten geraden Zahl“ als Entscheidungsregel: Wenn der Bruchteil 0,5 ist, rundet es zur nächsten geraden Zahl.

Für mehr Kontrolle könnte `toFixed()` Ihre erste Wahl sein, aber denken Sie daran, es gibt einen String zurück. Zurück in eine Zahl zu konvertieren, könnte ein zusätzlicher Schritt sein, stellt aber sicher, dass Sie weiterhin mit numerischen Typen arbeiten.

Alternativen? Bibliotheken wie `lodash` bieten `_.round(number, [precision=0])` für eine nuanciertere Kontrolle. Oder, das neuere `Intl.NumberFormat` bietet Ihnen hochpräzises Formatieren über das bloße Runden hinaus.

Apropos Präzision, achten Sie auf die Eigenheiten von Fließkommazahlen in JavaScript. `0.1 + 0.2` ist wegen der Art und Weise, wie Zahlen gespeichert werden, nicht genau `0.3`. Manchmal ist Runden notwendig, um solche Fließkommazahl-Fehler zu korrigieren.

## Siehe auch
- Mozillas Mathematikdokumentation: [MDN Web Docs](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Math)
- Finanzrunden mit `Intl.NumberFormat`: [ECMAScript Internationalization API](https://tc39.es/ecma402/#numberformat-objects)
- `lodash` Runden: [Lodash Docs](https://lodash.com/docs/4.17.15#round)
