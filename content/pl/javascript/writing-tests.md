---
title:                "Javascript: Tworzenie testów"
programming_language: "Javascript"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/writing-tests.md"
---

{{< edit_this_page >}}

## Dlaczego pisać testy?

Pisanie testów jest kluczowym elementem tworzenia wysokiej jakości kodu w javascripcie. Testy pozwalają nam na sprawdzenie czy nasza aplikacja działa poprawnie, a także pomagają w wykrywaniu błędów i zapobieganiu ich występowaniu w przyszłości. Dzięki testom możemy mieć większą pewność co do działania naszego kodu i łatwiej odnaleźć ewentualne problemy.

## Jak pisać testy?

Aby móc pisać testy w javascripcie, musimy użyć odpowiedniego narzędzia do tego celu. Najpopularniejszymi narzędziami są **Jest** i **Mocha**, które wspierają testowanie w javascripcie. Poniżej przedstawione są przykłady kodu z wykorzystaniem obu narzędzi.

### Przykład użycia Jest:

```Javascript
const add = require('./add'); // import funkcji którą chcemy przetestować

test('dodaj 2 i 3 i wynik powinien być równy 5', () => {
  expect(add(2, 3)).toBe(5); // oczekiwany wynik to 5
});
```
Output:
```
 PASS  test.js
  ✓ dodaj 2 i 3 i wynik powinien być równy 5 (2ms)
```

### Przykład użycia Mocha:

```Javascript
const assert = require('assert'); // import funkcji asercji
const subtract = require('./subtract'); // import funkcji którą chcemy przetestować

describe('Funkcja odejmowania', () => {
  it('powinna odejmować poprawnie', () => {
    assert.equal(subtract(5, 3), 2); // asercja sprawdzająca czy wynik jest poprawny
    assert.equal(subtract(10, 2), 8);
  });
});
```
Output:
```
  Funkcja odejmowania
    ✓ powinna odejmować poprawnie
```

## Głębszy wgląd

Pisanie testów jest skutecznym sposobem na zapewnienie wysokiej jakości kodu. Pozwala nam na szybkie wykrycie błędów i łatwiejszą możliwość refaktoryzacji. Warto także pamiętać o testowaniu różnych przypadków, aby nasze testy pokrywały jak najwięcej scenariuszy działania kodu.

## Zobacz także

- [Jest](https://jestjs.io/pl/)
- [Mocha](https://mochajs.org/)
- [Asembler JavaScript: Wprowadzenie do testowania jednostkowego](https://assembler.pl/testowanie-jednostkowe-w-javascript)