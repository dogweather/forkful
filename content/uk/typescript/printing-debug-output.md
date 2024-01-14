---
title:    "TypeScript: Виведення відлагоджувальних виводів"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Чому

У випадку якщо ви програмуєте на TypeScript, ви, напевно, часто бачите в браузері або консолі повідомлення про помилки, які важко зрозуміти. З допомогою друку вихідних даних ви можете легко відстежувати та оптимізувати ваш код, а також виявляти деякі помилки, які не відображаються при виконанні програми.

## Як користуватися друкуванням вихідних даних

Для того, щоб роздрукувати значення вашої змінної, просто використовуйте функцію `console.log()` у вашому коді:

```TypeScript
let number = 5;
console.log(number);
```

Таким чином, у вас з'явиться повідомлення у консолі зі значенням вашої змінної:

```
5
```

Це особливо корисно при знаходженні помилок, оскільки ви можете друкувати значення різних змінних та об'єктів, щоб перевірити, чи вони містять правильні дані.

## Глибинний аналіз друкування вихідних даних

З допомогою функції `console.log()` ви можете друкувати не лише прості значення, але й складніші об'єкти, масиви та багато іншого. Ви також можете використовувати спеціальні методи для більш зручного відображення даних, наприклад `console.table()` для виводу таблиці зі значеннями або `console.dir()` для виводу детальної інформаціїіз об'єктів. 

Наприклад, давайте роздрукуємо масив об'єктів користувачів:

```TypeScript
let users = [{name: 'Ivan', age: 25}, {name: 'Maria', age: 30}];

console.table(users);
```

Результат буде виглядати наступним чином:

```
┌─────────┬─────────┬─────┐
│ (index) │  name   │ age │
├─────────┼─────────┼─────┤
│    0    │ 'Ivan'  │ 25  │
│    1    │ 'Maria' │ 30  │
└─────────┴─────────┴─────┘
```

Також можна використовувати знак `%c` для зміни кольору тексту у виведених даних. Наприклад:

```TypeScript
console.log("%c Error!", "color: red; font-size: 1.2em;");
```

Це зробить повідомлення про помилку більш помітним та легше відмінити від інших повідомлень.

## Дивіться також

- [Console API](https://developer.mozilla.org/uk/docs/Web/API/console/)
- [Debugging with TypeScript](https://2ality.com/2015/09/debugging-with-typescript.html)
- [Mastering Console Debugging in JavaScript](https://blog.bitsrc.io/mastering-console-debugging-in-javascript-46c3bc013852)