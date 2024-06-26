---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:01:42.918446-07:00
description: "\u041A\u0430\u043A: Fish \u0438\u043C\u0435\u0435\u0442 \u0432\u0441\
  \u0442\u0440\u043E\u0435\u043D\u043D\u043E\u0435 \u043C\u0430\u0433\u0438\u0447\u0435\
  \u0441\u043A\u043E\u0435 \u0441\u0440\u0435\u0434\u0441\u0442\u0432\u043E \u0434\
  \u043B\u044F \u0442\u0430\u043A\u043E\u0433\u043E \u0440\u043E\u0434\u0430 \u0437\
  \u0430\u0434\u0430\u0447. \u0418\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0439\
  \u0442\u0435 \u0444\u0443\u043D\u043A\u0446\u0438\u044E `string`, \u043D\u0435 \u043F\
  \u043E\u0442\u0435\u044F. \u0412\u0437\u0433\u043B\u044F\u043D\u0438\u0442\u0435\
  \ \u043D\u0430 \u044D\u0442\u0438 \u0437\u0430\u043A\u043B\u0438\u043D\u0430\u043D\
  \u0438\u044F."
lastmod: '2024-03-13T22:44:45.814128-06:00'
model: gpt-4-0125-preview
summary: "Fish \u0438\u043C\u0435\u0435\u0442 \u0432\u0441\u0442\u0440\u043E\u0435\
  \u043D\u043D\u043E\u0435 \u043C\u0430\u0433\u0438\u0447\u0435\u0441\u043A\u043E\u0435\
  \ \u0441\u0440\u0435\u0434\u0441\u0442\u0432\u043E \u0434\u043B\u044F \u0442\u0430\
  \u043A\u043E\u0433\u043E \u0440\u043E\u0434\u0430 \u0437\u0430\u0434\u0430\u0447\
  ."
title: "\u0423\u0434\u0430\u043B\u0435\u043D\u0438\u0435 \u043A\u0430\u0432\u044B\u0447\
  \u0435\u043A \u0438\u0437 \u0441\u0442\u0440\u043E\u043A\u0438"
weight: 9
---

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
