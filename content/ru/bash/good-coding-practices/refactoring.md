---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:01:27.715632-07:00
description: "\u0420\u0435\u0444\u0430\u043A\u0442\u043E\u0440\u0438\u043D\u0433 -\
  \ \u044D\u0442\u043E \u043F\u0440\u043E\u0446\u0435\u0441\u0441 \u0440\u0435\u0441\
  \u0442\u0440\u0443\u043A\u0442\u0443\u0440\u0438\u0437\u0430\u0446\u0438\u0438 \u0441\
  \u0443\u0449\u0435\u0441\u0442\u0432\u0443\u044E\u0449\u0435\u0433\u043E \u043A\u043E\
  \u043C\u043F\u044C\u044E\u0442\u0435\u0440\u043D\u043E\u0433\u043E \u043A\u043E\u0434\
  \u0430 \u0431\u0435\u0437 \u0438\u0437\u043C\u0435\u043D\u0435\u043D\u0438\u044F\
  \ \u0435\u0433\u043E \u0432\u043D\u0435\u0448\u043D\u0435\u0433\u043E \u043F\u043E\
  \u0432\u0435\u0434\u0435\u043D\u0438\u044F. \u042D\u0442\u043E \u0436\u0438\u0437\
  \u043D\u0435\u043D\u043D\u043E \u0432\u0430\u0436\u043D\u0430\u044F \u043F\u0440\
  \u0430\u043A\u0442\u0438\u043A\u0430 \u0434\u043B\u044F\u2026"
lastmod: '2024-03-13T22:44:45.385854-06:00'
model: gpt-4-0125-preview
summary: "\u0420\u0435\u0444\u0430\u043A\u0442\u043E\u0440\u0438\u043D\u0433 - \u044D\
  \u0442\u043E \u043F\u0440\u043E\u0446\u0435\u0441\u0441 \u0440\u0435\u0441\u0442\
  \u0440\u0443\u043A\u0442\u0443\u0440\u0438\u0437\u0430\u0446\u0438\u0438 \u0441\u0443\
  \u0449\u0435\u0441\u0442\u0432\u0443\u044E\u0449\u0435\u0433\u043E \u043A\u043E\u043C\
  \u043F\u044C\u044E\u0442\u0435\u0440\u043D\u043E\u0433\u043E \u043A\u043E\u0434\u0430\
  \ \u0431\u0435\u0437 \u0438\u0437\u043C\u0435\u043D\u0435\u043D\u0438\u044F \u0435\
  \u0433\u043E \u0432\u043D\u0435\u0448\u043D\u0435\u0433\u043E \u043F\u043E\u0432\
  \u0435\u0434\u0435\u043D\u0438\u044F. \u042D\u0442\u043E \u0436\u0438\u0437\u043D\
  \u0435\u043D\u043D\u043E \u0432\u0430\u0436\u043D\u0430\u044F \u043F\u0440\u0430\
  \u043A\u0442\u0438\u043A\u0430 \u0434\u043B\u044F\u2026"
title: "\u0420\u0435\u0444\u0430\u043A\u0442\u043E\u0440\u0438\u043D\u0433"
---

{{< edit_this_page >}}

## Что и почему?
Рефакторинг - это процесс реструктуризации существующего компьютерного кода без изменения его внешнего поведения. Это жизненно важная практика для снижения сложности, повышения удобства обслуживания и поддержания вашей кодовой базы в здоровом и более понятном состоянии как для текущих, так и для будущих разработчиков.

## Как это делать:
Рассмотрим простой Bash-скрипт, который нуждается в рефакторинге. Он громоздкий, с повторяющимся кодом и его сложно следить:

```Bash
#!/bin/bash
echo "Введите имя файла:"
read filename
if [ -f "$filename" ]; then
    echo "Файл существует."
    count=$(grep -c "foo" "$filename")
    echo "Слово foo встречается $count раз."
else
    echo "Файл не существует."
fi
```

Рефакторинг для повышения ясности и возможности повторного использования может включать в себя введение функций и более изящную обработку ошибок:

```Bash
#!/bin/bash

function file_exists() {
    [[ -f "$1" ]]
}

function count_occurrences() {
    grep -c "$1" "$2"
}

function main() {
    local filename word count
    echo "Введите имя файла:"
    read -r filename
    echo "Введите искомое слово:"
    read -r word

    if file_exists "$filename"; then
        count=$(count_occurrences "$word" "$filename")
        echo "Слово $word встречается $count раз."
    else
        echo "Файл не существует." >&2
        exit 1
    fi
}

main "$@"
```

Рефакторированная версия использует функции для повышения читаемости и обеспечивает потенциальное повторное использование.

## Глубокое погружение:
Рефакторинг - это не концепция, которая появилась с Bash или даже с высокоуровневыми языками программирования; она стара, как само программирование. Термин был официально введен в книге "Refactoring: Improving the Design of Existing Code" Мартина Фаулера в 1999 году, сосредоточенной в основном на объектно-ориентированных языках.

В контексте Bash-скриптов рефакторинг часто означает разбиение длинных скриптов на функции, сокращение повторений с помощью циклов или условных операторов и избегание общих проблем, таких как неудачное обращение с пробелами в именах файлов. Альтернативы Bash для сценариев, которые стали слишком сложными, включают Python или Perl, которые предлагают лучшие структуры данных и обработку ошибок для сложных задач.

Рефакторинг, специфичный для Bash, больше связан с соблюдением лучших практик, таких как использование кавычек для переменных, использование `[[ ]]` для тестов вместо `[ ]`, и предпочтение `printf` вместо `echo` для надежного вывода. Детали реализации часто связаны с соблюдением руководств по стилю и использованием инструментов вроде `shellcheck` для статического анализа, чтобы выявлять распространенные ошибки.

## Смотрите также:
- [Руководство по стилю Shell от Google](https://google.github.io/styleguide/shellguide.html)
- [ShellCheck, инструмент статического анализа для shell-скриптов](https://www.shellcheck.net/)
- [Искусство командной строки](https://github.com/jlevy/the-art-of-command-line)
