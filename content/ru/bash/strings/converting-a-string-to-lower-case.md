---
title:                "Преобразование строки в нижний регистр"
aliases:
- ru/bash/converting-a-string-to-lower-case.md
date:                  2024-01-28T23:56:46.517707-07:00
model:                 gpt-4-0125-preview
simple_title:         "Преобразование строки в нижний регистр"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/bash/converting-a-string-to-lower-case.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Зачем?

Преобразование строки в нижний регистр - это преобразование всех символов алфавита в строке в их форму нижнего регистра. Программисты используют преобразование в нижний регистр для обеспечения единообразия, сравнения без учета регистра и для выполнения требований системы или приложения.

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
