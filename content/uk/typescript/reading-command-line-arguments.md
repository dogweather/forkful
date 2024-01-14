---
title:    "TypeScript: Читання аргументів командного рядка"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Чому
Люди користуються аргументами командного рядка, щоб забезпечити більш гнучку та інтерактивну роботу з програмами. Це дозволяє програмі бути більш універсальною та пристосованою до різноманітних умов та потреб користувачів.

## Як
В TypeScript використання аргументів командного рядка досить просте. Необхідно створити функцію, яка буде отримувати аргументи з командного рядка та виводити їх. Приклад коду та виводу:

```TypeScript
function readCommandLineArgs() {
   const args = process.argv;
   console.log("Аргументи командного рядка:")
   for (let i = 2; i < args.length; i++) {
      console.log(args[i]);
   }
}
```

Виклик функції із наступними аргументами командного рядка:

```TypeScript
node index.js hello world
```

Вивід:

```
Аргументи командного рядка:
hello
world
```

## Deep Dive
Для кращого розуміння роботи аргументів командного рядка в TypeScript, давайте трохи поглибимося в теорію. У Node.js існує глобальна змінна `process`, яка дозволяє взаємодіяти з даними процесу, що запущений в даний момент. Вона містить в собі різноманітні властивості, серед яких є і `argv`.

Масив `argv` містить аргументи командного рядка, передані при запуску скрипта. При цьому, першим елементом масиву завжди буде шлях до виконуваного файла, а наступними будуть передані аргументи.

Також, варто зазначити, що аргументи командного рядка завжди буде отримувати тип `string`. Тому у деяких випадках може бути потрібно перетворити їх на необхідний тип (наприклад, число) за допомогою відповідних методів JavaScript.

## See Also
- [Робота з аргументами командного рядка в Node.js](https://nodejs.org/docs/latest-v13.x/api/process.html#process_process_argv)
- [Типи в TypeScript](https://www.typescriptlang.org/docs/handbook/basic-types.html)
- [Перетворення типів в TypeScript](https://www.typescriptlang.org/docs/handbook/basic-types.html#type-assertions)