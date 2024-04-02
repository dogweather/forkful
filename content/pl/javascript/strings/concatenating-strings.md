---
date: 2024-01-20 17:35:01.305431-07:00
description: "\u0141\u0105czenie ci\u0105g\xF3w znak\xF3w, czyli \"concatenating strings\"\
  , pozwala sklei\u0107 ze sob\u0105 dwie lub wi\u0119cej r\xF3\u017Cnych cz\u0119\
  \u015Bci tekstu. Programi\u015Bci robi\u0105 to, aby tworzy\u0107\u2026"
lastmod: '2024-03-13T22:44:35.788450-06:00'
model: gpt-4-1106-preview
summary: "\u0141\u0105czenie ci\u0105g\xF3w znak\xF3w, czyli \"concatenating strings\"\
  , pozwala sklei\u0107 ze sob\u0105 dwie lub wi\u0119cej r\xF3\u017Cnych cz\u0119\
  \u015Bci tekstu. Programi\u015Bci robi\u0105 to, aby tworzy\u0107\u2026"
title: "\u0141\u0105czenie \u0142a\u0144cuch\xF3w znak\xF3w"
weight: 3
---

## Co i dlaczego?
Łączenie ciągów znaków, czyli "concatenating strings", pozwala skleić ze sobą dwie lub więcej różnych części tekstu. Programiści robią to, aby tworzyć zdania, wiadomości lub dynamicznie konstruować treść wyświetlaną użytkownikowi.

## Jak to zrobić?
Oto kilka sposobów na łączenie ciągów znaków w JavaScript:

```javascript
// Użycie operatora +
let greeting = "Cześć, " + "jak się masz?";
console.log(greeting); // "Cześć, jak się masz?"

// Użycie metod szablonu (template literals)
let name = "Ania";
let question = `Hej ${name}, kiedy idziemy na kawę?`;
console.log(question); // "Hej Ania, kiedy idziemy na kawę?"

// Użycie metody concat()
let part1 = "Do zobaczenia";
let part2 = " jutro!";
let farewell = part1.concat(part2);
console.log(farewell); // "Do zobaczenia jutro!"
```

## Zagłębiamy się
Początkowo, w pierwszych wersjach JavaScript, operator `+` był głównym sposobem na łączenie ciągów znaków. Wraz z ES6, wprowadzone zostały szablony literałów (template literals), które uprościły łączenie ciągów dodając możliwość interpolacji.

Inne języki programowania często posiadają dedykowane funkcje do łączenia ciągów, a JavaScript oferuje metodę `.concat()`, aczkolwiek jest ona rzadziej używana, gdyż operator `+` czy literały szablonowe są wygodniejsze w codziennym użyciu.

W keście wydajności, operator `+` i literały szablonowe mają podobną wydajność, która jest zazwyczaj wystarczająca dla typowych zastosowań. Metody `.concat()` można w niektórych przypadkach używać, kiedy łączy się bardzo dużą liczbę ciągów znaków, ale różnica ta jest zauważalna tylko w skrajnych przypadkach.

## Zobacz również
- MDN Web Docs na temat łączenia ciągów: https://developer.mozilla.org/pl/docs/Web/JavaScript/Reference/Global_Objects/String/concat
- Szablony literałów (template literals) na MDN: https://developer.mozilla.org/pl/docs/Web/JavaScript/Reference/Template_literals
- Wydajność różnych metod łączenia ciągów znaków: https://jsperf.com/concat-vs-plus-vs-join
