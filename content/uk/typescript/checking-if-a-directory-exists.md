---
title:                "Перевірка наявності директорії"
html_title:           "Go: Перевірка наявності директорії"
simple_title:         "Перевірка наявності директорії"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Що і чому?

Перевірка на існування директорії - це процедура, яка дозволяє програмам виявити, чи існує конкретна директорія у файловій системі. Програмісти роблять це, щоб уникнути помилок при спробах читання, запису або модифікації неіснуючої директорії.

## Як це зробити:

Щоб перевірити, чи існує директорія, використовуйте модуль `fs` у Node.js та його метод `existsSync()`. Приклад використання:

```TypeScript
import * as fs from 'fs';

let directory = '/mydirectory';

if (fs.existsSync(directory)) {
    console.log('Директорія існує!');
} else {
    console.log('Директорія не існує!');
}
```

Якщо директорія існує, цей код виведе "Директорія існує!", інакше - "Директорія не існує!".

## Поглиблений розбір: 

1) Історичний контекст: Перевірка на існування директорії - це важлива процедура, яка використовувалася ще з початку персональної комп'ютерної індустрії, коли розмір дисків був обмежений, і не розкривався до помилок вводу-виводу.

2) Альтернативи: Можна використовувати `fs.promises.access()`, що є проміс-версією перевірки на існування.

3) Деталі реалізації: `fs.existsSync()` синхронно перевіряє існування директорії, блокуючи виконання програми, доки не отримає відповідь. У випадках інтенсивного використання може впливати на продуктивність.

## Дивіться також:

1) [FS - FileSystem Node.js v16.9.1](https://nodejs.org/api/fs.html)
2) [Тема на StackOverflow, що обговорює fs.existsSync() vs fs.promises.access()](https://stackoverflow.com/questions/4981891/node-js-equivalent-of-pythons-os-path-exists)
3) [Туториал про роботу з файлами та директоріями в TypeScript](https://www.digitalocean.com/community/tutorials/how-to-work-with-files-in-node-js)