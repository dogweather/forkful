---
title:                "Pisanie testów"
date:                  2024-01-19
html_title:           "Bash: Pisanie testów"
simple_title:         "Pisanie testów"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/writing-tests.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Testowanie kodu to sprawdzanie, czy nasz program działa jak oczekujemy. Robimy to, aby uniknąć błędów i zagwarantować jakość kodu na każdym etapie rozwoju.

## Jak to zrobić?
W JavaScript używamy frameworków do testowania, np. Jest. Oto prosty przykład testu funkcji sumującej dwie liczby:

```javascript
// sum.js
function sum(a, b) {
  return a + b;
}
module.exports = sum;

// sum.test.js
const sum = require('./sum');

test('dodaje 1 + 2 dając wynik 3', () => {
  expect(sum(1, 2)).toBe(3);
});
```

Uruchamiamy testy komendą `npm test`. Otrzymujemy wynik:

```plaintext
PASS ./sum.test.js
✓ dodaje 1 + 2 dając wynik 3 (5ms)
```

## Głębsze spojrzenie
Testowanie kodu w JavaScript zaczęło nabierać na znaczeniu w ostatnich latach, szczególnie z trendem na rozwój aplikacji webowych. Oprócz Jest, popularne są też Mocha, Jasmine oraz QUnit. Każde narzędzie ma swoje specyfiki, ale podstawowa idea pozostaje ta sama – wykonanie kodu w kontrolowanych warunkach i sprawdzenie rezultatów działania. W przypadku Jest, zyskujemy dodatkowe korzyści jak m.in. mockowanie zależności i snapshot testing.

## Zobacz również
- [Jest](https://jestjs.io/) – oficjalna strona Jest, z dokumentacją i poradnikami.
- [Mocha](https://mochajs.org/) – strona Mocha, z przykładami użycia.
- [Jasmine](https://jasmine.github.io/) – dokumentacja Jasmin.
- [QUnit](https://qunitjs.com/) – strona QUnit dla testowania kodu JavaScript.
