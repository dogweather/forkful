---
title:                "Fish Shell: Читання аргументів командного рядка"
simple_title:         "Читання аргументів командного рядка"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Чому

Є багато причин, чому вивчати обробку аргументів командного рядка в Fish Shell. Це дає змогу ефективніше керувати вашими програмами, тестувати та налагоджувати їх, а також збільшує загальну продуктивність.

## Як

Для отримання аргументів командного рядка в Fish Shell, ви можете використовувати змінну `$argv`. Наприклад:

```Fish Shell
echo "Перший аргумент: " $argv[1]
echo "Другий аргумент: " $argv[2]
```

Вибір окремих аргументів можна також здійснити на основі їх індексу, як показано у прикладі вище. Не забудьте використовувати лапки для отримання рядкових аргументів.

## Глибинний аналіз

У Fish Shell є кілька вкладених змінних, які допоможуть вам отримати доступ до певної інформації про аргументи командного рядка. Наприклад, `$argc` містить кількість аргументів загалом, а `$argv0` містить назву виконуваного файлу.

Також використання умов в Fish Shell дозволяє створювати більш динамічні програми, що обробляють різні варіанти аргументів. Для цього можете скористатися конструкцією `switch`:

```Fish Shell
switch $argv[1]
case -h or --help
	echo "Довідкова інформація"
case -v or --version
	echo "Версія програми"
case "*"
	echo "Невідома команда"
end
```

## Дивіться також

- [Офіційна документація Fish Shell](https://fishshell.com/docs/current/cmds/set.html)
- [Розділ про обробку аргументів командного рядка у Fish Shell Cookbook](https://fishshell.com/docs/current/index.html#user_aliases)
- [Стаття про використання умов в Fish Shell](https://ryanstutorials.net/bash-scripting-tutorial/bash-if-statements.php)