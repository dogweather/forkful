---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:01:43.487302-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u0437\u0430\u043C\u0435\
  \u043D\u0438\u043C \u0432\u0441\u0435 \u0432\u0445\u043E\u0436\u0434\u0435\u043D\
  \u0438\u044F \u0441\u043B\u043E\u0432\u0430 'cat' \u043D\u0430 'dog' \u0432 \u0441\
  \u0442\u0440\u043E\u043A\u0435."
lastmod: '2024-03-13T22:44:45.808881-06:00'
model: gpt-4-0125-preview
summary: "\u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u0437\u0430\u043C\u0435\u043D\
  \u0438\u043C \u0432\u0441\u0435 \u0432\u0445\u043E\u0436\u0434\u0435\u043D\u0438\
  \u044F \u0441\u043B\u043E\u0432\u0430 'cat' \u043D\u0430 'dog' \u0432 \u0441\u0442\
  \u0440\u043E\u043A\u0435."
title: "\u041F\u043E\u0438\u0441\u043A \u0438 \u0437\u0430\u043C\u0435\u043D\u0430\
  \ \u0442\u0435\u043A\u0441\u0442\u0430"
weight: 10
---

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
