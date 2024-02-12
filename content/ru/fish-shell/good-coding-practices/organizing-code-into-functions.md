---
title:                "Организация кода в функции"
aliases: - /ru/fish-shell/organizing-code-into-functions.md
date:                  2024-01-28T23:59:35.484804-07:00
model:                 gpt-4-0125-preview
simple_title:         "Организация кода в функции"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/fish-shell/organizing-code-into-functions.md"
changelog:
  - 2024-01-28, dogweather, reviewed and added links
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Организация кода в функции заключается в объединении фрагментов сценария для выполнения конкретных задач. Мы делаем это потому, что это упрощает чтение кода, его тестирование и повторное использование — никому не хочется пробираться сквозь болото кода-спагетти.

## Как это сделать:
В Fish вы пишете функцию с ключевым словом `function`, даёте ей имя и заканчиваете `end`. Вот простой пример:

```fish
function hello
    echo "Привет, мир!"
end

hello
```

Вывод:
```
Привет, мир!
```

Теперь давайте сделаем так, чтобы она приветствовала пользователя:

```fish
function greet
    set user (whoami)
    echo "Приветики, $user!"
end

greet
```

Вывод:
```
Приветики, ваш_пользователь!
```

Чтобы сохранить функцию на будущие сеансы, используйте `funcsave greet`.

## Погружение
Функции Fish Shell похожи на мини-скрипты — вы можете поместить туда практически что угодно. Исторически концепция функций в скриптах оболочки сэкономила бесчисленное количество часов повторяющегося набора и отладки. В отличие от языков программирования, таких как Python, функции Shell больше ориентированы на удобство, чем на структуру.

Некоторые оболочки, такие как Bash, используют `function` или просто фигурные скобки. Fish придерживается `function ... end` — чётко и понятно. Внутри функций Fish у вас есть все удобства: параметры, локальные переменные с `set -l`, и вы даже можете определить функцию внутри другой функции.

Вам не понадобится значение `return`, потому что Fish не сильно заботится об этом; вывод вашей функции и есть её возвращаемое значение. И если вы хотите, чтобы функции оставались доступными для будущих сеансов, помните о `funcsave`.

## Смотрите также

- Учебник по функциям fish: [https://fishshell.com/docs/current/tutorial.html#tut_functions](https://fishshell.com/docs/current/tutorial.html#functions)

### Команды для работы с функциями

- [function](https://fishshell.com/docs/current/cmds/function.html) — Создать функцию
- [functions](https://fishshell.com/docs/current/cmds/functions.html) — Печатать или стирать функции
- [funcsave](https://fishshell.com/docs/current/cmds/funcsave.html) — Сохранить определение функции в каталог автозагрузки пользователя
- [funced](https://fishshell.com/docs/current/cmds/funced.html) — Интерактивно редактировать функцию
