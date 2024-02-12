---
title:                "Удаление кавычек из строки"
date:                  2024-01-29T00:01:42.918446-07:00
model:                 gpt-4-0125-preview
simple_title:         "Удаление кавычек из строки"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/fish-shell/removing-quotes-from-a-string.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Удаление кавычек из строки заключается в очищении ваших текстовых данных от этих надоедливых одинарных (' ') или двойных (" ") кавычек. Программисты часто делают это для санитарии ввода или подготовки данных к дальнейшей обработке без беспорядка, вызванного кавычками.

## Как:

Fish имеет встроенное магическое средство для такого рода задач. Используйте функцию `string`, не потея. Взгляните на эти заклинания:

```fish
# Пример с одинарными кавычками
set quoted "'Привет, Мир!'"
set unquoted (string trim --chars \"\'\" $quoted)
echo $unquoted # Вывод: Привет, Мир!

# Та же история с двойными кавычками
set double_quoted "\"Привет, Вселенная!\""
set unquoted (string trim --chars \"\'\" $double_quoted)
echo $unquoted # Вывод: Привет, Вселенная!
```

## Подробнее

В эпоху каменного века командной строки вы боролись бы с `sed` или `awk`, чтобы удалить кавычки; настоящий клубок обратных слэшей и загадочных флагов. Функция `string` в Fish относится к более новой эре, делая код чище и более интуитивно понятным.

Альтернативы в других оболочках могут до сих пор полагаться на эти старые инструменты или использовать свои собственные встроенные методы, например, расширение параметров в bash или модификаторы в zsh.

Функция `string` выходит за рамки обрезки кавычек. Это швейцарский нож для операций со строками в Fish. С помощью `string`, вы можете нарезать, разбивать, соединять или даже сопоставлять с регулярными выражениями строки прямо в вашем терминале.

## См. также

Погрузитесь глубже в `string` с помощью официальной документации:
- [Документация по строкам в Fish Shell](https://fishshell.com/docs/current/commands.html#string)

Для ностальгии или при написании сценариев с использованием более традиционных оболочек, проверьте:
- [Руководство по Sed & Awk](https://www.grymoire.com/Unix/Sed.html)
- [Расширение параметров в Bash](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)