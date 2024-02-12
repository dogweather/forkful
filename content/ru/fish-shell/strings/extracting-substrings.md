---
title:                "Извлечение подстрок"
aliases:
- /ru/fish-shell/extracting-substrings.md
date:                  2024-01-28T23:57:41.308859-07:00
model:                 gpt-4-0125-preview
simple_title:         "Извлечение подстрок"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/fish-shell/extracting-substrings.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?
Извлечение подстрок означает выделение конкретных частей строки. Программисты делают это для изоляции данных, очистки ввода или разбора информации для дальнейшей обработки.

## Как это сделать:
В Fish используйте команду `string` для манипуляции со строками. Вот как:

### Взять с начала:
```Fish Shell
set my_string "Fish Shell is fun!"
echo $my_string | string sub -l 4 # Выводит 'Fish'
```

### Обрезать с конца:
```Fish Shell
set my_string "Fish Shell is fun!"
echo $my_string | string sub -s -4 # Выводит 'fun!'
```

### Конкретный диапазон:
```Fish Shell
set my_string "Fish Shell is fun!"
echo $my_string | string sub -s 6 -l 5 # Выводит 'Shell'
```

## Погружение в детали
В старые времена мы бы нарезали и манипулировали строками в Fish с использованием внешних инструментов, таких как `cut`, `awk` или `sed`. Теперь `string` – это наш основной встроенный инструмент, представленный в Fish 2.3.0. Он быстрее, более понятный и интегрируется бесшовно с нашими скриптами.

`string sub` - это не единственный вариант. Другие функции `string` могут разделять строки, заменять части или объединять их. Этот фокус на минимальном использовании ресурсов и легкости понимания.

Что касается реализации, когда вы извлекаете подстроки, Fish считывает строку и выводит только указанную вами часть, при этом уважая кодировку символов и избегая общих ошибок при извлечении подстрок, например, разделения символа пополам.

## Смотрите также
- Официальная документация Fish по `string`: https://fishshell.com/docs/current/cmds/string.html
- Учебные материалы сообщества по скриптованию в Fish: https://fishshell.com/docs/current/tutorial.html
- Обсуждения на Stack Overflow о манипуляции со строками в Fish: https://stackoverflow.com/questions/tagged/fish
