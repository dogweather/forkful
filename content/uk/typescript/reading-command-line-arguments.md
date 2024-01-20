---
title:                "Читання аргументів командного рядка"
html_title:           "Arduino: Читання аргументів командного рядка"
simple_title:         "Читання аргументів командного рядка"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Прочитаємо аргументи командного рядка в TypeScript!

## Що й навіщо?
Читання аргументів командного рядка - це процес, коли ваша програма збирає ввід користувача при виклику команди, тобто переде вашею програмою. Це робить ваш код більш гнучким і більш реактивним на дії користувача.

## Як це робити:
Більшість програм в Node.js отримує аргументи командного рядка через об'єкт `process.argv`. В TypeScript це може виглядати так:
```TypeScript
const args = process.argv.slice(2); // Відрізаємо перші два аргументи
console.log(args)
```
Запустіть цей код із додатковими параметрами, щоб побачити результат:
```
$ node yourscript.ts firstArg secondArg thirdArg
[ 'firstArg', 'secondArg', 'thirdArg' ]
```

## Занурення глибше:
Історично мови, такі як C і Perl, використовували хитру специфікацію "argv" для передачі аргументів командного рядка. Node.js схожий на ці мови. Багато модулей Node.js, як-от `minimist` і `yargs`, виходять за рамки базового `process.argv` для створення більш зручного аналізу аргументів.

## Дивись також:
- [модуль Node.js `yargs`](https://www.npmjs.com/package/yargs)
- [модуль Node.js `minimist`](https://www.npmjs.com/package/minimist)
- [Передача аргументів командного рядка в Node.js (EN)](https://nodejs.dev/learn/nodejs-accept-arguments-from-the-command-line)
- [Аргументи командного рядка в C (EN)](https://www.gnu.org/software/libc/manual/html_node/Program-Arguments.html#Program-Arguments)