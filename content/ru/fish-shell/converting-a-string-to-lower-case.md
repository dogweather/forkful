---
title:                "Преобразование строки в нижний регистр"
date:                  2024-01-28T23:56:23.075328-07:00
model:                 gpt-4-0125-preview
simple_title:         "Преобразование строки в нижний регистр"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/fish-shell/converting-a-string-to-lower-case.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Зачем?

Преобразование строки в нижний регистр изменяет все буквы в этой строке на их форму в нижнем регистре. Программисты делают это для обеспечения консистентности, сравнения, сортировки или чтобы соответствовать требованиям к регистрозависимости некоторых систем.

## Как это сделать:

Используя команду `string`, преобразование текста в нижний регистр происходит прямолинейно. Просто выполните:

```Fish Shell
echo "MAKE ME LOWERCASE" | string lower
```

Пример вывода:

```
make me lowercase
```

Для переменной:

```Fish Shell
set my_string "SHOUTY CASE TEXT"
string lower -q -- $my_string
```

Вывод:

```
shouty case text
```

## Глубокое погружение:

До появления Fish Shell пользователи Unix часто использовали `tr '[:upper:]' '[:lower:]'` или `awk '{print tolower($0)}'`. Хотя эти способы работают, они не такие чистые или прямолинейные, как встроенная функция Fish `string lower`.

Fish ввел команду `string` в версии 2.3.0 (май 2016), повысив манипуляцию со строками до уровня основной части оболочки, вместо необходимости использования внешних команд. Это добавило простоту и скорость к таким общим задачам, как преобразование регистра.

Почему не использовать просто `tr` или `awk`? `string lower` встроено в Fish, что означает, что оно работает быстрее (не нужно создавать новые процессы) и работает последовательно и предсказуемо на различных системах. Кроме того, это часть более широкого набора команд `string`, которые обрабатывают другие операции со строками, что может сделать написание скриптов более аккуратным и эффективным.

## Смотрите также:

- Официальная документация для `string`: https://fishshell.com/docs/current/cmds/string.html
- Репозиторий Fish Shell в GitHub: https://github.com/fish-shell/fish-shell
- Исторический контекст и сравнение `string` с традиционными командами Unix: https://github.com/fish-shell/fish-shell/issues/159