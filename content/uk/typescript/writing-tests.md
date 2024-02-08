---
title:                "Письмо тестів"
aliases:
- uk/typescript/writing-tests.md
date:                  2024-02-03T19:32:31.946725-07:00
model:                 gpt-4-0125-preview
simple_title:         "Письмо тестів"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?
Написання тестів на TypeScript включає створення автоматизованих скриптів для перевірки функціональності та коректності вашого коду. Програмісти роблять це, щоб забезпечити надійність, швидко знаходити помилки та сприяти підтримуваності зростання коду, оскільки статична типізація TypeScript додає рівень передбачуваності до тестування JavaScript.

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
