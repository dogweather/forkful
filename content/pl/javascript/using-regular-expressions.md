---
title:                "Wykorzystanie wyrażeń regularnych"
date:                  2024-01-19
simple_title:         "Wykorzystanie wyrażeń regularnych"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Wyrażenia regularne to wzorce służące do wyszukiwania i manipulacji tekstami. Programiści używają ich, by łatwo znajdować, porządkować i edytować dane tekstowe, szczególnie przy dużych objętościach lub złożonych strukturach danych.

## How to: (Jak to zrobić?)
```javascript
// Znalezienie numerów w tekście
let tekst = "Zamówienie 123, pozycja 456.";
let regExpNumer = /\d+/g;
console.log(tekst.match(regExpNumer)); // Output: ['123', '456']

// Zamiana danych w tekście
let maskownieEmaili = (email) => email.replace(/(\w+)@(\w+)\.(\w+)/g, '***@***.***');
console.log(maskownieEmaili("jan.kowalski@example.com")); // Output: '***@***.***'

// Weryfikacja formatu kodu pocztowego
let kodPocztowy = '00-700';
let regExpKod = /^\d{2}-\d{3}$/;
console.log(regExpKod.test(kodPocztowy)); // Output: true
```

## Deep Dive (Głębsze spojrzenie)
Wyrażenia regularne, znane jako regex, sięgają lat 50. XX wieku. Były częścią składnika teoretycznej informatyki - teorii automatów i języków formalnych. W JavaScript, wyrażenia regularne są obiektami klasy `RegExp`. Istnieją alternatywy do regexów, takie jak funkcje `indexOf` czy `includes`, ale żadna z nich nie oferuje takiej elastyczności. Wyrażenia regularne obrabiają stringi na niskim poziomie, co może wpłynąć na wydajność; wartość rozpoznać potrzebę ich używania.

## See Also (Zobacz także)
- MDN Web Docs na temat wyrażeń regularnych: [MDN RegExp](https://developer.mozilla.org/pl/docs/Web/JavaScript/Reference/Global_Objects/RegExp)
- Tutorial do wyrażeń regularnych w JavaScript: [JavaScript RegExp Tutorial](https://www.regular-expressions.info/javascript.html)
- Interaktywny tester wyrażeń regularnych: [RegExr](https://regexr.com/)
