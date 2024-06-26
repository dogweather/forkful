---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:57:04.630096-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0412 Fish Shell \u043C\u0430\u0433\u0438\u044F \u043F\u0440\u043E\
  \u0438\u0441\u0445\u043E\u0434\u0438\u0442 \u0441 \u043F\u043E\u043C\u043E\u0449\
  \u044C\u044E \u0443\u0442\u0438\u043B\u0438\u0442\u044B `string`, \u0443\u0434\u043E\
  \u0431\u043D\u043E\u0433\u043E \u0432\u0441\u0442\u0440\u043E\u0435\u043D\u043D\u043E\
  \u0433\u043E \u0438\u043D\u0441\u0442\u0440\u0443\u043C\u0435\u043D\u0442\u0430\
  \ \u0434\u043B\u044F \u0440\u0430\u0431\u043E\u0442\u044B \u0441\u043E \u0441\u0442\
  \u0440\u043E\u043A\u0430\u043C\u0438 - \u043F\u0440\u0435\u0434\u0441\u0442\u0430\
  \u0432\u043B\u0435\u043D\u043D\u043E\u0433\u043E \u0432\u2026"
lastmod: '2024-04-05T21:53:46.153073-06:00'
model: gpt-4-0125-preview
summary: "\u0412 Fish Shell \u043C\u0430\u0433\u0438\u044F \u043F\u0440\u043E\u0438\
  \u0441\u0445\u043E\u0434\u0438\u0442 \u0441 \u043F\u043E\u043C\u043E\u0449\u044C\
  \u044E \u0443\u0442\u0438\u043B\u0438\u0442\u044B `string`, \u0443\u0434\u043E\u0431\
  \u043D\u043E\u0433\u043E \u0432\u0441\u0442\u0440\u043E\u0435\u043D\u043D\u043E\u0433\
  \u043E \u0438\u043D\u0441\u0442\u0440\u0443\u043C\u0435\u043D\u0442\u0430 \u0434\
  \u043B\u044F \u0440\u0430\u0431\u043E\u0442\u044B \u0441\u043E \u0441\u0442\u0440\
  \u043E\u043A\u0430\u043C\u0438 - \u043F\u0440\u0435\u0434\u0441\u0442\u0430\u0432\
  \u043B\u0435\u043D\u043D\u043E\u0433\u043E \u0432 \u0432\u0435\u0440\u0441\u0438\
  \u0438 2.3.0."
title: "\u0423\u0434\u0430\u043B\u0435\u043D\u0438\u0435 \u0441\u0438\u043C\u0432\u043E\
  \u043B\u043E\u0432, \u0441\u043E\u043E\u0442\u0432\u0435\u0442\u0441\u0442\u0432\
  \u0443\u044E\u0449\u0438\u0445 \u0448\u0430\u0431\u043B\u043E\u043D\u0443"
weight: 5
---

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
