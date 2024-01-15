---
title:                "Написання тестів"
html_title:           "TypeScript: Написання тестів"
simple_title:         "Написання тестів"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/writing-tests.md"
---

{{< edit_this_page >}}

Привіт усім! Як ви вже, напевно, знаєте, написання тестів є важливою складовою у сучасному програмуванні. Однак, чому ж це так важливо? Як це зробити у TypeScript? Давайте розглянемо це разом у цій статті.

## Чому

Написання тестів допомагає нам переконаємося, що наш код працює так, як ми очікуємо. Це дозволяє нам виявити будь-які помилки та більш зручно вдосконалювати наш код у майбутньому. Крім того, це також допомагає зберегти час, оскільки ми можемо швидко перевірити наш код без необхідності вручну тестувати його кожного разу після змін.

## Як це зробити

Для початку, ми повинні встановити пакет `Jest` за допомогою нашого менеджера пакетів. Далі, ми можемо створити файли для наших тестів і використовувати різні функції `expect` для перевірки наших результатів. Давайте подивимося на приклад коду:

```TypeScript
import { sum } from './math';

describe('Math functions', () => {
  test('sum function adds two numbers', () => {
    expect(sum(2, 2)).toBe(4);
  });

  test('sum function returns null when one parameter is missing', () => {
    expect(sum(10)).toBeNull();
  });
});
```

Як бачите, ми використовуємо функцію `describe` для групування наших тестів та функцію `test` для перевірки наших результатів. Для очікування певних результатів ми використовуємо функцію `expect` разом з функціями, такими як `toEqual` або `toBe`. Коли ми запускаємо наші тести, ми повинні бачити щось подібне до цього:

```TypeScript
 PASS  math.test.ts
  Math functions
    ✓ sum function adds two numbers (1ms)
    ✓ sum function returns null when one parameter is missing (1ms)

Test Suites: 1 passed, 1 total
Tests:       2 passed, 2 total
Snapshots:   0 total
Time:        0.681s
```

## Deep Dive

Якщо ви хочете дізнатися більше про написання тестів у TypeScript, вам стане в нагоді документація на офіційному сайті `Jest`. Також, варто розглянути книгу "Test-Driven Development with TypeScript", яка може допомогти вам розібратися з практичними прикладами.

## Дивись також

- [Документація `Jest`](https://jestjs.io/)
- ["Тестово-орієнтоване програмування з TypeScript"](https://amzn.to/3iUBvM8)