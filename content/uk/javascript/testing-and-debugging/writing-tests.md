---
title:                "Письмо тестів"
date:                  2024-02-03T19:31:42.123543-07:00
model:                 gpt-4-0125-preview
simple_title:         "Письмо тестів"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?

Написання тестів на JavaScript означає створення автоматизованих скриптів, які запускають ваш код, щоб переконатися, що він працює як очікується. Це може значно покращити надійність та зручність обслуговування ваших додатків. Програмісти роблять це, щоб виявити помилки на ранньому етапі, полегшити рефакторинг коду та переконатися, що нові функції не порушують існуючу функціональність.

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
