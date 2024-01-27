---
title:                "Написання тестів"
date:                  2024-01-19
html_title:           "Arduino: Написання тестів"
simple_title:         "Написання тестів"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (Що й Чому?)
Тести - це перевірки коду, щоб впевнитися, він працює правильно. Програмісти пишуть тести для зниження помилок, економії часу на довгострокову перспективу та підтримки якості.

## How to: (Як це робити:)
```TypeScript
// Встановлення Jest
// npm install --save-dev jest @types/jest ts-jest

// sum.ts
export function sum(a: number, b: number): number {
  return a + b;
}

// sum.test.ts
import { sum } from './sum';

test('adds 1 + 2 to equal 3', () => {
  expect(sum(1, 2)).toBe(3);
});

// Запуск тестів:
// npx jest
```

Sample Output:
```
PASS  ./sum.test.ts
✓ adds 1 + 2 to equal 3 (5ms)
```

## Deep Dive (Детальний огляд):
Історичний контекст: TDD (Test-Driven Development) з'явився у 90-х. Він закликає до написання тестів до написання самого коду.
Альтернативи: Мімо Jest, інші бібліотеки включають Mocha, Jasmine та Ava.
Деталі реалізації: Jest - це бібліотека з моками, покриттям коду та знущальним тестуванням. Вона підтримує конкурентні тести та легко інтегрується з TypeScript.

## See Also (Дивіться також):
- Jest документація: https://jestjs.io/docs/getting-started
- Тестування TypeScript з Jest: https://kulshekhar.github.io/ts-jest
- TDD визначення: https://www.agilealliance.org/glossary/tdd/
