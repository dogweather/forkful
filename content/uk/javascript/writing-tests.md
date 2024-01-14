---
title:                "Javascript: Написання тестів"
simple_title:         "Написання тестів"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/writing-tests.md"
---

{{< edit_this_page >}}

## Для чого
Написання тестів є важливою складовою процесу програмування, оскільки допомагає підтвердити правильність роботи коду та запобігає появі потенційних помилок.

## Як
Для початку, імпортуємо бібліотеку для тестування у файл проекту:
```Javascript
const assert = require('assert');
```

Наприклад, наступний код перевіряє правильність роботи функції знаходження суми двох чисел:
```Javascript
function findSum(a, b) {
  return a + b;
}

assert.equal(findSum(2, 3), 5);
```

Якщо функція працює правильно, тест пройде успішно, а якщо є які-небудь помилки - тест видасть повідомлення про невдачу.

## Deep Dive
Написання тестів дозволяє забезпечити надійність та стабільність роботи програмного продукту. Крім того, вони полегшують процес розробки, оскільки допомагають виявити проблеми та помилки в коді на ранніх етапах.

Також, справна система тестування може використовуватися як документація для нових розробників, які вперше знайомляться з проектом.

## Дивись також
- [Методологія тестування використана в Javascript](https://uk.wikipedia.org/wiki/Selenium)
- [Візуальні бібліотеки для тестування у Javascript](https://www.testim.io/blog/javascript-testing-libraries/)
- [Порівняння різних фреймворків для тестування у Javascript](https://www.sitepoint.com/javascript-unit-testing-frameworks/)