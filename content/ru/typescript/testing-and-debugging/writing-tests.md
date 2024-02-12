---
title:                "Написание тестов"
aliases: - /ru/typescript/writing-tests.md
date:                  2024-01-29T00:06:15.558588-07:00
model:                 gpt-4-0125-preview
simple_title:         "Написание тестов"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/typescript/writing-tests.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Зачем?

Написание тестов - это создание кода, который проверяет, правильно ли работает другой код. Программисты делают это, чтобы заранее обнаруживать ошибки, экономить время и убедиться, что изменения не ломают функционал.

## Как это делать:

Давайте протестируем простую функцию с использованием Jest, популярного фреймворка для тестирования в JavaScript и TypeScript.

Сначала установите Jest с поддержкой TypeScript:

```bash
npm install --save-dev jest @types/jest ts-jest
```

Добавьте `jest.config.js`:

```js
module.exports = {
  preset: 'ts-jest',
  testEnvironment: 'node',
};
```

Определите функцию в `math.ts`:

```typescript
export function add(a: number, b: number): number {
  return a + b;
}
```

Напишите тест в `math.test.ts`:

```typescript
import { add } from './math';

test('прибавляет 1 + 2, чтобы получить 3', () => {
  expect(add(1, 2)).toBe(3);
});
```

Запустите тесты:

```bash
npx jest
```

Пример вывода:

```
PASS  ./math.test.ts
✓ прибавляет 1 + 2, чтобы получить 3 (5ms)
```

## Подробнее

Тестирование в TypeScript строится на практиках тестирования в JavaScript. Вот что делает его особенным:

- Исторический контекст: TypeScript появился в 2012 году. Его задачей было добавление типизации в JavaScript, что делает код легче в поддержке и тестировании.
- Альтернативы: Кроме Jest, есть Mocha, Jasmine и многие другие. У каждого есть уникальные особенности; выбирайте в зависимости от своих потребностей.
- Детали реализации: Тесты могут находиться как рядом с кодом, так и отдельно. Типы TypeScript помогают с автодополнением и добавляют уверенности в тесты.

## Смотрите также

- Jest: [Документация по Jest](https://jestjs.io/docs/getting-started)
- Сравнение фреймворков для тестирования JS: [Опрос StateOfJS 2022](https://2022.stateofjs.com/en-US/libraries/testing/)
