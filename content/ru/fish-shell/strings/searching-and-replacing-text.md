---
aliases:
- /ru/fish-shell/searching-and-replacing-text/
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:01:43.487302-07:00
description: "\u041F\u043E\u0438\u0441\u043A \u0438 \u0437\u0430\u043C\u0435\u043D\
  \u0430 \u0442\u0435\u043A\u0441\u0442\u0430 \u0437\u0430\u043A\u043B\u044E\u0447\
  \u0430\u0435\u0442\u0441\u044F \u0432 \u043D\u0430\u0445\u043E\u0436\u0434\u0435\
  \u043D\u0438\u0438 \u043E\u043F\u0440\u0435\u0434\u0435\u043B\u0451\u043D\u043D\u044B\
  \u0445 \u0441\u0442\u0440\u043E\u043A \u0438 \u0438\u0445 \u0437\u0430\u043C\u0435\
  \u043D\u0435 \u043D\u0430 \u043D\u0435\u0447\u0442\u043E \u0438\u043D\u043E\u0435\
  . \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B \u0434\
  \u0435\u043B\u0430\u044E\u0442 \u044D\u0442\u043E \u0434\u043B\u044F \u043E\u0431\
  \u043D\u043E\u0432\u043B\u0435\u043D\u0438\u044F \u043A\u043E\u0434\u0430, \u0438\
  \u0441\u043F\u0440\u0430\u0432\u043B\u0435\u043D\u0438\u044F\u2026"
lastmod: 2024-02-18 23:08:57.492635
model: gpt-4-0125-preview
summary: "\u041F\u043E\u0438\u0441\u043A \u0438 \u0437\u0430\u043C\u0435\u043D\u0430\
  \ \u0442\u0435\u043A\u0441\u0442\u0430 \u0437\u0430\u043A\u043B\u044E\u0447\u0430\
  \u0435\u0442\u0441\u044F \u0432 \u043D\u0430\u0445\u043E\u0436\u0434\u0435\u043D\
  \u0438\u0438 \u043E\u043F\u0440\u0435\u0434\u0435\u043B\u0451\u043D\u043D\u044B\u0445\
  \ \u0441\u0442\u0440\u043E\u043A \u0438 \u0438\u0445 \u0437\u0430\u043C\u0435\u043D\
  \u0435 \u043D\u0430 \u043D\u0435\u0447\u0442\u043E \u0438\u043D\u043E\u0435. \u041F\
  \u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B \u0434\u0435\u043B\
  \u0430\u044E\u0442 \u044D\u0442\u043E \u0434\u043B\u044F \u043E\u0431\u043D\u043E\
  \u0432\u043B\u0435\u043D\u0438\u044F \u043A\u043E\u0434\u0430, \u0438\u0441\u043F\
  \u0440\u0430\u0432\u043B\u0435\u043D\u0438\u044F\u2026"
title: "\u041F\u043E\u0438\u0441\u043A \u0438 \u0437\u0430\u043C\u0435\u043D\u0430\
  \ \u0442\u0435\u043A\u0441\u0442\u0430"
---

{{< edit_this_page >}}

## Что и Почему?
Поиск и замена текста заключается в нахождении определённых строк и их замене на нечто иное. Программисты делают это для обновления кода, исправления ошибок или для переформатирования данных — это значительно экономит время.

## Как это сделать:
Давайте заменим все вхождения слова 'cat' на 'dog' в строке.

```Fish Shell
echo "One cat, two cats, three cats." | string replace -a 'cat' 'dog'
```
Пример вывода:
```
One dog, two dogs, three dogs.
```
Замена текста в файле с именем `pets.txt`:

```Fish Shell
string replace -a 'cat' 'dog' < pets.txt > updated_pets.txt
```

Использование переменных для шаблонов:

```Fish Shell
set old "cat"
set new "dog"
string replace -a $old $new < pets.txt > updated_pets.txt
```

## Погружение в детали
Поиск и замена текста существуют в текстовых редакторах с незапамятных времён. Вспомните `sed` для потокового редактирования в Unix — это старая добрая классика. Fish поднимает это на новый уровень, делая процесс проще с помощью команды `string`. Больше нет необходимости мучиться с регулярными выражениями, если только вы этого не хотите. Альтернативы? Конечно: `sed`, `awk`, скрипты на Perl, даже макросы `vim`. Но команда `string` в Fish выделяется своей элегантностью и меньшей подверженностью ошибкам для обычных задач.

## Смотрите также:
- Официальная документация Fish Shell по команде `string`: [fishshell.com/docs/current/cmds/string.html](https://fishshell.com/docs/current/cmds/string.html)
- Sed на примерах, часть 1: [https://www.gnu.org/software/sed/manual/sed.html](https://www.gnu.org/software/sed/manual/sed.html)
- Программирование на языке AWK — Строковые Функции: [https://www.gnu.org/software/gawk/manual/gawk.html#String-Functions](https://www.gnu.org/software/gawk/manual/gawk.html#String-Functions)
