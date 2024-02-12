---
title:                "Pisanie testów"
date:                  2024-02-03T19:31:40.492830-07:00
model:                 gpt-4-0125-preview
simple_title:         "Pisanie testów"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?

Pisanie testów w JavaScript odnosi się do praktyki tworzenia automatycznych skryptów, które uruchamiają twój kod, aby upewnić się, że działa on zgodnie z oczekiwaniami, co może znacznie poprawić niezawodność i możliwość utrzymania aplikacji. Programiści robią to, aby wychwycić błędy na wczesnym etapie, ułatwić refaktoryzację kodu i zapewnić, że nowe funkcje nie zakłócą istniejącej funkcjonalności.

## Jak to zrobić:

### Natywne podejście (używając Jest)

Jest to popularne narzędzie do testowania, które dostarcza przyjazne API do pisania testów jednostkowych w JavaScript. Wymaga minimalnej konfiguracji i oferuje funkcje takie jak atrapy funkcji, timery i testowanie migawek.

1. **Instalacja**:

```bash
npm install --save-dev jest
```

2. **Pisanie prostego testu**:

Utwórz plik o nazwie `sum.test.js`:

```javascript
const sum = require('./sum'); // Zakładamy, że ta funkcja po prostu dodaje dwie liczby

test('dodaje 1 + 2, żeby otrzymać 3', () => {
  expect(sum(1, 2)).toBe(3);
});
```

3. **Uruchamianie testu**:

```bash
npx jest
```

**Przykładowe wyjście:**

```plaintext
PASS  ./sum.test.js
✓ dodaje 1 + 2, żeby otrzymać 3 (5ms)
```

### Testowanie kodu asynchronicznego

Jest ułatwia testowanie obietnic i składni async/await:

```javascript
// asyncSum.js
async function asyncSum(a, b) {
  return Promise.resolve(a + b);
}

// asyncSum.test.js
test('asynchroniczne dodawanie działa', async () => {
  await expect(asyncSum(1, 2)).resolves.toBe(3);
});

```

### Użycie bibliotek stron trzecich (Mocha & Chai)

Mocha to kolejne popularne narzędzie do testowania, często używane z biblioteką asercji Chai dla bardziej ekspresyjnych testów.

1. **Instalacja**:

```bash
npm install --save-dev mocha chai
```

2. **Pisanie testu z wykorzystaniem Mochy i Chai**:

Utwórz `calculate.test.js`:

```javascript
const chai = require('chai');
const expect = chai.expect;

const calculate = require('./calculate'); // Prosty moduł kalkulacyjny

describe('Calculate', function() {
  it('powinien sumować dwie wartości', function() {
    expect(calculate.sum(5, 2)).to.equal(7);
  });
});
```

3. **Uruchamianie testów z Mocha**:

Dodaj skrypt w swoim pliku `package.json`:

```json
"scripts": {
  "test": "mocha"
}
```

Następnie wykonaj:

```bash
npm test
```

**Przykładowe wyjście:**

```plaintext
  Calculate
    ✓ powinien sumować dwie wartości


  1 passing (8ms)
```

Te przykłady ilustrują podstawy pisania i wykonywania testów w JavaScript. Przyjęcie ramy do testowania takiej jak Jest czy Mocha z Chai może zapewnić solidną podstawę do solidnego testowania aplikacji, pomagając zapewnić, że twój kod funkcjonuje zgodnie z zamierzeniami przez aktualizacje i refaktoryzacje.
