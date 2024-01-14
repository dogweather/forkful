---
title:    "Javascript: Читання аргументів командного рядка"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Навіщо

Програмісти часто використовують командні рядки для запуску своїх програм або взаємодії з ними. Читання аргументів командного рядка дозволяє управляти програмою під час її виконання шляхом передачі параметрів у командну рядок.

## Як

```Javascript
// Приклад коду для читання аргументів командного рядка
const args = process.argv.slice(2); // перший і другий аргументи є необов'язковими і не містять потрібної інформації
console.log('Перший параметр: ' + args[0]); // виведе перший аргумент
console.log('Другий параметр: ' + args[1]); // виведе другий аргумент (якщо переданий)
```

Якщо ви запустите цей код через командний рядок з параметрами, наприклад, 'node index.js hello world', то ви побачите наступний результат:

```
Перший параметр: hello
Другий параметр: world
```

В цьому прикладі ми використали `process.argv` для отримання масиву всіх переданих командній рядку аргументів, а потім використали `slice ()` для видалення перших двох аргументів, які не є потрібними для нашої програми.

## Deep Dive

При роботі з командним рядком важливо розуміти, що отримані аргументи будуть у форматі рядка. Тому, якщо вам потрібно використовувати їх як числа, вам потрібно буде сконвертувати їх відповідним чином, використовуючи методи `parseInt` або `parseFloat` (або інші відповідні методи, якщо вам потрібно конвертувати до інших типів даних).

Також важливо розуміти порядок, в якому аргументи будуть у масиві `process.argv`. Вони будуть в такому порядку, в якому їх вказано в командному рядку. Наприклад, якщо ви введете 'node index.js world hello', то перший аргумент буде 'world', а другий - 'hello'.

## Диверсифікація

- [Документація Node.js про `process.argv`](https://nodejs.org/dist/latest-v12.x/docs/api/process.html#process_process_argv)
- [Northwestern University tutorial про читання командних аргументів в Node.js](https://www.northwestern.edu/code/node/command-line-arguments.html)
- [Стаття Medium про використання командних аргументів в Node.js](https://medium.com/@theguus/using-command-line-arguments-in-node-js-4686d3b81e58)

## Дивіться також

- [Блог про розробку програмного забезпечення на Node.js](https://exampleblog.com)
- [Інший корисний блог про програмування на Javascript](https://exampleblog.com)