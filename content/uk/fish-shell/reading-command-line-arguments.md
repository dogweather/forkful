---
title:                "Читання аргументів командного рядка"
html_title:           "Fish Shell: Читання аргументів командного рядка"
simple_title:         "Читання аргументів командного рядка"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Чому

Цей стаття призначена для тих, хто хоче дізнатися, як працюють аргументи командного рядка у Fish Shell. Це може бути корисно для покращення продуктивності та зрозуміння принципів роботи цього шелу.

## Як

Для того, щоб прочитати аргументи командного рядка у Fish Shell, використовуйте вбудовану функцію `fish_prompt`. У наступному прикладі можна побачити, як це виглядає у дії:

```Fish Shell
fish_prompt -u
```

Цей код виведе ім’я поточного користувача, що дає змогу перевірити його, якщо у вас є багато користувачів на системі.

## Глибше

Для того, щоб докладніше розібратися з аргументами командного рядка у Fish Shell, можна використати функцію `fish_process_substitutions`. Вона повертає масив, який містить усі аргументи командного рядка. Ось приклад використання цієї функції:

```Fish Shell
set args (fish_process_substitutions)
echo $args
```

Цей код виведе усі аргументи, які були передані у командний рядок при виконанні цього сценарію.

## Дивись Також

- [Офіційна документація Fish Shell](https://fishshell.com/docs/current/index.html)
- [Портал спільноти Fish Shell у Discord](https://discord.gg/fish)