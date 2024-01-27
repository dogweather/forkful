---
title:                "Pisanie testów"
date:                  2024-01-19
html_title:           "Bash: Pisanie testów"
simple_title:         "Pisanie testów"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
Testowanie to pisania kodu, który sprawdza czy inny kod działa jak powinien. Programiści tworzą testy, aby zapewnić jakość i uniknąć błędów w przyszłości.

## How to:
Zainstaluj bibliotekę do testowania, np. Jest, używając `npm`:

```bash
npm install --save-dev jest @types/jest ts-jest
```

Konfiguruj `jest` dodając w pliku `jest.config.js`:

```javascript
module.exports = {
  preset: 'ts-jest',
  testEnvironment: 'node',
};
```

Twórz test dla prostego modułu:

```TypeScript
// sum.ts
export function sum(a: number, b: number): number {
  return a + b;
}
```

```TypeScript
// sum.test.ts
import { sum } from './sum';

test('dodaje 1 + 2, żeby otrzymać 3', () => {
  expect(sum(1, 2)).toBe(3);
});
```

Uruchom testy:

```bash
npx jest
```

Wynik powinien wyglądać mniej więcej tak:

```plaintext
PASS ./sum.test.ts
√ dodaje 1 + 2, żeby otrzymać 3 (5ms)
```

## Deep Dive
Testowanie w TypeScript zaczęło się nie długo po jego wynalezieniu w 2012 roku – jako naturalna ekstensja podejścia ze świata JavaScript. Oprócz Jest, inne popularne biblioteki to Mocha, Jasmine czy AVA. Detale implementacyjne zależą od wybranej biblioteki, ale idee pozostają podobne – tworzenie asercji, które potwierdzają, że kod zachowuje się oczekiwanie.

## See Also
- Oficjalna dokumentacja TypeScript: https://www.typescriptlang.org/
- Dokumentacja Jest: https://jestjs.io/
- Porównanie narzędzi do testowania w TypeScript: https://raygun.com/blog/javascript-testing-frameworks/
