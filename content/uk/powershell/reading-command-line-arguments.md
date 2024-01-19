---
title:                "Читання аргументів командного рядка"
html_title:           "PowerShell: Читання аргументів командного рядка"
simple_title:         "Читання аргументів командного рядка"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Що і Чому?

Отримання командного рядка параметрів - це процес, у якому програма читає вхідну інформацію, надану користувачем в командному рядку під час запуску програми. Це потрібно для того, щоб програми могли працювати з різними вхідними даними і виконувати різні завдання в залежності від цих даних.

## Як це зробити:

```PowerShell
$arg1 = $args[0] # змінна arg1 буде містити перший введений параметр
$arg2 = $args[1] # змінна arg2 буде містити другий введений параметр
Write-Host "Перший параметр: $arg1"
Write-Host "Другий параметр: $arg2"
```

Результат виконання програми з введеними параметрами "hello" та "world":

```PowerShell
Перший параметр: hello
Другий параметр: world
```

## Глибоке дослідження:

Отримання командного рядка параметрів є стандартною функцією у багатьох мовах програмування, а також у PowerShell. Це дозволяє програмістам створювати більш гнучкі та корисні програми, які можуть працювати з різними вхідними даними.

Є кілька альтернативних методів отримання командного рядка параметрів, зокрема використання відповідних бібліотек для обробки вхідних аргументів. Однак, використання вбудованих засобів мови програмування є простим та ефективним способом досягнення цієї функціональності.

Для зчитування параметрів можна використовувати не тільки нумеровані змінні, як показано в прикладі вище, але іменовані параметри за допомогою спеціального параметру -Param. Це дозволяє більш зручно та чітко передавати вхідні дані програмі.

## Дивись також:

Офіційна документація PowerShell про отримання командного рядка параметрів: https://docs.microsoft.com/en-us/powershell/scripting/learn/ps101/02-getting-input?view=powershell-7.1