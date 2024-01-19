---
title:                "Читання аргументів командного рядка"
html_title:           "Arduino: Читання аргументів командного рядка"
simple_title:         "Читання аргументів командного рядка"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Що та навіщо?

Командні аргументи рядків - це параметри, які передаються до програми під час її запуску в консолі. Програмісти часто використовують їх для керування поведінкою програми, роблячи її більш гнучкою і кастомізованою.

## Як це зробити:

Можна отримати доступ до командних аргументів рядків в Node.js, використовуючи вбудований модуль `process.argv`. Ось простий приклад:

```Javascript
console.log(process.argv);
```

При запуску цієї програми з аргументами `node app.js one two`, виведеться:

```Javascript 
[ '/usr/local/bin/node', '/path/to/your/script.js', 'one', 'two' ]
```

## Поглиблений огляд:

Цей спосіб отримання аргументів рядка команд в сучасних версіях JavaScript виник ще з початкових версій Node.js.

Альтернативи включають використання бібліотек, таких як commander.js або yargs, які дають більше можливостей для аналізу та використання аргументів.

Реалізація `process.argv` в Node.js бере аргументи командного рядка, додає перші два значення (шлях до поточного виконавця Node.js і файлу скрипту), а потім додає всі додаткові аргументи як масив рядків.

## Додаткова Інформація:

- Документація Node.js по process.argv: https://nodejs.org/docs/latest/api/process.html#process_process_argv 
- Пакет commander.js на npm: https://www.npmjs.com/package/commander 
- Пакет yargs на npm: https://www.npmjs.com/package/yargs