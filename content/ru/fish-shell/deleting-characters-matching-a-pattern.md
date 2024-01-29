---
title:                "Удаление символов, соответствующих шаблону"
date:                  2024-01-28T23:57:04.630096-07:00
model:                 gpt-4-0125-preview
simple_title:         "Удаление символов, соответствующих шаблону"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/fish-shell/deleting-characters-matching-a-pattern.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Удаление символов, соответствующих шаблону, фактически является фильтрацией нежелательных символов или последовательностей из строк или содержимого файлов на основе правил, известных как шаблоны. Программисты делают это для очистки данных, подготовки их к обработке или для извлечения значимой информации без лишних данных.

## Как это сделать:

```Fish Shell
# Удалить цифры из строки
set string "Fish123Shell"
echo $string | string replace -ra '[0-9]' ''
# Выводит: FishShell

# Удалить все кроме строчных букв
set noisy_string "F!i@s#h$%S^h&e*l(l)__+"
echo $noisy_string | string match -r '[a-z]+'
# Выводит: ishhell
```

## Подробнее

В Fish Shell магия происходит с помощью утилиты `string`, удобного встроенного инструмента для работы со строками - представленного в версии 2.3.0. До этого пользователи обращались к стандартным инструментам UNIX, таким как `sed` или `awk`. Почему изменения? Простота и интеграция. Наличие собственного решения упрощает манипуляции со строками, делая скрипты более читаемыми и удобными для поддержки.

Альтернативы? Конечно, старая добрая утилита `sed` все еще может справиться с задачей:

```Fish Shell
set old_school_string "Fish@Shell2023"
echo $old_school_string | sed 's/[0-9]//g'
# Выводит: Fish@Shell
```

Но почему бы не использовать собственные инструменты Fish? Для реализации `string replace` имеет опцию `-r`, позволяющую использовать регулярные выражения. `-a` применяет команду ко всем совпадениям, а добавление '' в конце говорит о том, чтобы заменить на ничего, т. е., удалить. Используйте `string match`, когда ищете шаблон для сохранения, а не для удаления.

## Смотрите также

- Официальная документация Fish Shell о `string`: https://fishshell.com/docs/current/cmds/string.html
- Учебник по регулярным выражениям для глубокого погружения в шаблоны: https://www.regular-expressions.info/
- Sed & Awk, вечные текстовые силы: введение: https://www.gnu.org/software/sed/manual/sed.html, http://www.grymoire.com/Unix/Awk.html
