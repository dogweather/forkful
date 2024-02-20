---
date: 2024-01-20 17:46:11.797273-07:00
description: "I JavaScript er uttrekk av delstrenger som et mini-felttog for \xE5\
  \ hente spesifikke tekstbiter fra st\xF8rre strenger. Vi gj\xF8r dette for \xE5\
  \ manipulere,\u2026"
lastmod: 2024-02-19 22:05:00.449024
model: gpt-4-1106-preview
summary: "I JavaScript er uttrekk av delstrenger som et mini-felttog for \xE5 hente\
  \ spesifikke tekstbiter fra st\xF8rre strenger. Vi gj\xF8r dette for \xE5 manipulere,\u2026"
title: Uthenting av delstrenger
---

{{< edit_this_page >}}

## What & Why?
I JavaScript er uttrekk av delstrenger som et mini-felttog for å hente spesifikke tekstbiter fra større strenger. Vi gjør dette for å manipulere, analysere eller bare vise frem biter av data som er relevante for oppgaven vår.

## How to:
```javascript
let fullString = "Hei verden, jeg lærer JavaScript!";
let substring = fullString.substring(4, 10);  // "verden"

console.log(substring);  // Output: "verden"

// Eller bruk slice for et lignende resultat:
let sliced = fullString.slice(4, 10);  // "verden"
console.log(sliced);  // Output: "verden"

// For dynamiske situasjoner, kanskje indexOf + substring:
let searchTerm = "verden";
let startIndex = fullString.indexOf(searchTerm);
let endIndex = startIndex + searchTerm.length;
let foundSubstring = fullString.substring(startIndex, endIndex);  // "verden"

console.log(foundSubstring);  // Output: "verden"
```

## Deep Dive
Bak i tida, når Netscape navigerte veien, introduserte de `substring`. Siden da, 'slice' og moderne 'String' metoder kom, utvidet språket. Du lurer på forskjellen? `substring` og `slice` kan virke utbyttbare, men `slice` kan ta negative indekser, tar seg en runde bakfra. I forhold til ytelse, ingen store alarmklokker; valget er mer om lesbarhet og preferanse.

Hvis du ser etter noe mer robust, kan `substr` (med en "b") ha vært på radaren, men se opp—den regnes som utdatert og er ikke anbefalt for ny kode.

`indexOf` er nyttig for å finne startpunktet til et utdrag når du ikke vet posisjonen på forhånd. Kombinert med `substring` eller `slice`, blir det en dynamisk duo for strengmanipulasjon.

## See Also
- MDN Web Docs on `substring()`: [MDN substring](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- MDN Web Docs on `slice()`: [MDN slice](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
- A deeper dive into string methods: [JavaScript String Methods](https://www.w3schools.com/js/js_string_methods.asp)
