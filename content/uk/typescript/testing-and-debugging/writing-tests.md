---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:31.946725-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : TypeScript \u0433\u0430\u0440\u043C\u043E\u043D\u0456\u0439\u043D\u043E \u043F\
  \u0440\u0430\u0446\u044E\u0454 \u0437 \u0431\u0456\u043B\u044C\u0448\u0456\u0441\
  \u0442\u044E \u0444\u0440\u0435\u0439\u043C\u0432\u043E\u0440\u043A\u0456\u0432\
  \ \u0434\u043B\u044F \u0442\u0435\u0441\u0442\u0443\u0432\u0430\u043D\u043D\u044F\
  \ JavaScript. \u0414\u043B\u044F \u043D\u0430\u043E\u0447\u043D\u043E\u0441\u0442\
  \u0456 \u043C\u0438 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u0454\
  \u043C\u043E Jest, \u043F\u043E\u043F\u0443\u043B\u044F\u0440\u043D\u0438\u0439\
  \ \u0444\u0440\u0435\u0439\u043C\u0432\u043E\u0440\u043A\u2026"
lastmod: '2024-03-13T22:44:48.872460-06:00'
model: gpt-4-0125-preview
summary: "TypeScript \u0433\u0430\u0440\u043C\u043E\u043D\u0456\u0439\u043D\u043E\
  \ \u043F\u0440\u0430\u0446\u044E\u0454 \u0437 \u0431\u0456\u043B\u044C\u0448\u0456\
  \u0441\u0442\u044E \u0444\u0440\u0435\u0439\u043C\u0432\u043E\u0440\u043A\u0456\u0432\
  \ \u0434\u043B\u044F \u0442\u0435\u0441\u0442\u0443\u0432\u0430\u043D\u043D\u044F\
  \ JavaScript."
title: "\u041F\u0438\u0441\u044C\u043C\u043E \u0442\u0435\u0441\u0442\u0456\u0432"
weight: 36
---

## Як це зробити:
TypeScript гармонійно працює з більшістю фреймворків для тестування JavaScript. Для наочності ми використаємо Jest, популярний фреймворк для тестування, через його налаштування без конфігурації для проектів TypeScript.

Спочатку переконайтеся, що у вас встановлені Jest та необхідні типи TypeScript:

```bash
npm install --save-dev jest typescript ts-jest @types/jest
```

Далі, налаштуйте Jest для роботи з TypeScript, змінивши `jest.config.js` або створивши новий:

```javascript
module.exports = {
  preset: 'ts-jest',
  testEnvironment: 'node',
};
```

Тепер давайте напишемо просту функцію та тест для неї. Розглянемо файл `sum.ts` з наступною функцією:

```typescript
// sum.ts
export function sum(a: number, b: number): number {
  return a + b;
}
```

Створіть тестовий файл під назвою `sum.test.ts`:

```typescript
// sum.test.ts
import { sum } from './sum';

test('додає 1 + 2, щоб отримати 3', () => {
  expect(sum(1, 2)).toBe(3);
});
```

Запустіть свої тести з:

```bash
npx jest
```

Приклад виводу, який вказує на пройдений тест, має виглядати так:

```plaintext
 PASS  ./sum.test.ts
  ✓ додає 1 + 2, щоб отримати 3 (2 ms)
```

Для асинхронного коду Jest підтримує `async/await`. Припустимо, у вас є асинхронна функція `fetchData`:

```typescript
// asyncFunctions.ts
export async function fetchData(): Promise<string> {
  return "data";
}
```

Ваш тест із використанням асинхронних функцій:

```typescript
// asyncFunctions.test.ts
import { fetchData } from './asyncFunctions';

test('успішно отримує дані', async () => {
  expect(await fetchData()).toBe('data');
});
```

Під час запуску ваших тестів Jest дочекається вирішення промісу, правильно тестуючи асинхронні операції.

Пам'ятайте, ефективне тестування включає написання декількох тестів для різних сценаріїв, включаючи крайні випадки, щоб забезпечити очікувану поведінку вашого коду на TypeScript.
