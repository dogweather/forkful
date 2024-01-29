---
title:                "Поиск и замена текста"
date:                  2024-01-29T00:01:43.487302-07:00
model:                 gpt-4-0125-preview
simple_title:         "Поиск и замена текста"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/fish-shell/searching-and-replacing-text.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
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
