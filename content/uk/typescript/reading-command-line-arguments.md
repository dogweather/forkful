---
title:                "Читання параметрів командного рядка"
html_title:           "TypeScript: Читання параметрів командного рядка"
simple_title:         "Читання параметрів командного рядка"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Чому

Читання аргументів командного рядка є важливою навичкою для будь-якого програміста, оскільки це дозволяє збільшити ефективність і зручність роботи з програмами через термінал.

## Як це зробити

```TypeScript
// Приклад коду з використанням бібліотеки commander для зчитування аргументів
import * as commander from 'commander';

commander
  .version('1.0.0')
  .option('-p, --port <number>', 'Specify port')
  .option('-s, --server <string>', 'Specify server')
  .parse(process.argv);

if (commander.port) {
  // Виконуємо код з використанням порта, переданого в аргументах
  console.log(`Server running on port ${commander.port}`);
}

if (commander.server) {
  // Виконуємо код з використанням сервера, переданого в аргументах
  console.log(`Connecting to server: ${commander.server}`);
}
```

Використання бібліотеки commander дозволяє створити потужні інтерфейси для зчитування аргументів командного рядка, допускає передачу параметрів через флаги або позиційні аргументи і надає можливість виконувати додаткові дії залежно від отриманої інформації.

## Глибоке дослідження

Щоб краще розібратися зі зчитуванням аргументів командного рядка, важливо зрозуміти як саме вони передаються в програму. В більшості випадків, аргументи передаються через об'єкт `process.argv`, який містить масив с.Т.р., які були передані при запуску програми. Цей масив містить не тільки назву файла, але і всі аргументи, передані через флаги або позиційні аргументи. Для зручного зчитування цих аргументів можна використовувати бібліотеку commander або писати власний код, який працюватиме з масивом аргументів.

## Подивіться також

- [Бібліотека commander] (https://www.npmjs.com/package/commander)
- [Документація TypeScript] (https://www.typescriptlang.org/docs/home.html)