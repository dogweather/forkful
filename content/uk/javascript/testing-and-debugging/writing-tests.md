---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:42.123543-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : #."
lastmod: '2024-03-13T22:44:49.999196-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "\u041F\u0438\u0441\u044C\u043C\u043E \u0442\u0435\u0441\u0442\u0456\u0432"
weight: 36
---

## Як це зробити:


### Нативний підхід (використовуючи Jest)
Jest — це популярний фреймворк для тестування, який надає зручний API для написання юніт-тестів на JavaScript. Він вимагає мінімальної конфігурації і має такі функції, як мок-функції, таймери та тестування по знімках.

1. **Інсталяція**:

```bash
npm install --save-dev jest
```

2. **Написання простого тесту**:

Створіть файл `sum.test.js`:

```javascript
const sum = require('./sum'); // Припускаємо, що ця функція просто додає два числа

test('додає 1 + 2, щоб отримати 3', () => {
  expect(sum(1, 2)).toBe(3);
});
```

3. **Запуск вашого тесту**:

```bash
npx jest
```

**Приклад виводу**:

```plaintext
PASS  ./sum.test.js
✓ додає 1 + 2, щоб отримати 3 (5ms)
```

### Тестування асинхронного коду
Jest полегшує тестування промісів та синтаксису async/await:

```javascript
// asyncSum.js
async function asyncSum(a, b) {
  return Promise.resolve(a + b);
}

// asyncSum.test.js
test('асинхронне додавання працює', async () => {
  await expect(asyncSum(1, 2)).resolves.toBe(3);
});

```

### Використання сторонніх бібліотек (Mocha та Chai)
Mocha — це ще один популярний фреймворк для тестування, який часто використовують разом із бібліотекою перевірок Chai для більш виразних тестів.

1. **Інсталяція**:

```bash
npm install --save-dev mocha chai
```

2. **Написання тесту за допомогою Mocha та Chai**:

Створіть `calculate.test.js`:

```javascript
const chai = require('chai');
const expect = chai.expect;

const calculate = require('./calculate'); // Простий модуль обчислення

describe('Calculate', function() {
  it('повинен сумувати два значення', function() {
    expect(calculate.sum(5, 2)).to.equal(7);
  });
});
```

3. **Запуск ваших тестів з Mocha**:

Додайте скрипт у ваш `package.json`:

```json
"scripts": {
  "test": "mocha"
}
```

Потім виконайте:

```bash
npm test
```

**Приклад виводу**:

```plaintext
  Calculate
    ✓ повинен сумувати два значення


  1 passing (8ms)
```

Ці приклади ілюструють базове написання тестів та їх виконання в JavaScript. Адаптація фреймворку для тестування, як-от Jest або Mocha з Chai, може забезпечити міцну основу для ретельного тестування додатків, допомагаючи забезпечити правильну роботу вашого коду при оновленнях та рефакторингах.
