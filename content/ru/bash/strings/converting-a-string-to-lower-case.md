---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:56:46.517707-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0434\u0435\u043B\u0430\u0442\
  \u044C: \u0412\u043E\u0442 \u043F\u0440\u043E\u0441\u0442\u043E\u0439 \u0441\u043F\
  \u043E\u0441\u043E\u0431 \u043F\u0440\u0435\u043E\u0431\u0440\u0430\u0437\u043E\u0432\
  \u0430\u0442\u044C \u0441\u0442\u0440\u043E\u043A\u0443 \u0432 \u043D\u0438\u0436\
  \u043D\u0438\u0439 \u0440\u0435\u0433\u0438\u0441\u0442\u0440 \u0432 Bash."
lastmod: '2024-03-13T22:44:45.341611-06:00'
model: gpt-4-0125-preview
summary: "\u0412\u043E\u0442 \u043F\u0440\u043E\u0441\u0442\u043E\u0439 \u0441\u043F\
  \u043E\u0441\u043E\u0431 \u043F\u0440\u0435\u043E\u0431\u0440\u0430\u0437\u043E\u0432\
  \u0430\u0442\u044C \u0441\u0442\u0440\u043E\u043A\u0443 \u0432 \u043D\u0438\u0436\
  \u043D\u0438\u0439 \u0440\u0435\u0433\u0438\u0441\u0442\u0440 \u0432 Bash."
title: "\u041F\u0440\u0435\u043E\u0431\u0440\u0430\u0437\u043E\u0432\u0430\u043D\u0438\
  \u0435 \u0441\u0442\u0440\u043E\u043A\u0438 \u0432 \u043D\u0438\u0436\u043D\u0438\
  \u0439 \u0440\u0435\u0433\u0438\u0441\u0442\u0440"
weight: 4
---

## Как это делать:
Вот простой способ преобразовать строку в нижний регистр в Bash:

```Bash
str="Make Me Lower Case"
lower_str=$(echo "$str" | tr '[:upper:]' '[:lower:]')

echo $lower_str
```

Вывод:

```
make me lower case
```

В Bash версии 4.0 и выше существует встроенный способ с использованием расширения параметра:

```Bash
str="Make Me Lower Case"
lower_str="${str,,}"

echo $lower_str
```

Вывод:

```
make me lower case
```

## Подробнее
До Bash 4.0 для преобразования строк в нижний регистр часто использовались внешние утилиты, такие как `tr`, `awk` или `sed`. Каждая из них предоставляет различные способы манипуляции со строками, не ограничиваясь только изменением регистра, но может требовать запуска нового процесса, что влияет на производительность.

Введение синтаксиса `${parameter,,pattern}` в Bash 4.0 предоставило встроенную функцию для преобразования строк, которая работает быстрее и не зависит от внешних утилит. Есть альтернативы внутри самого Bash:

1. `awk`: `echo $str | awk '{print tolower($0)}'`
2. `sed`: `echo $str | sed 's/[A-Z]/\L&/g'`
3. `tr`: `echo $str | tr '[:upper:]' '[:lower:]'` - как показано выше.

С точки зрения реализации, `${parameter,,pattern}` изменяют не только символы ASCII; они осведомлены о UTF-8 и могут обрабатывать неанглийские символы, что делает их универсальными для международных приложений.

## Смотрите также
- Расширение параметров Bash: https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html
- Команда `tr`: https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html
- Программирование в AWK: https://www.gnu.org/software/gawk/manual/gawk.html
- Редактор потоков `sed`: https://www.gnu.org/software/sed/manual/sed.html
