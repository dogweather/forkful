---
date: 2024-01-26 01:16:52.826973-07:00
description: "\u042F\u043A: \u0420\u043E\u0437\u0433\u043B\u044F\u043D\u0435\u043C\
  \u043E \u043F\u0440\u043E\u0441\u0442\u0438\u0439 Bash \u0441\u043A\u0440\u0438\u043F\
  \u0442, \u044F\u043A\u0438\u0439 \u043F\u043E\u0442\u0440\u0435\u0431\u0443\u0454\
  \ \u0440\u0435\u0444\u0430\u043A\u0442\u043E\u0440\u0438\u043D\u0433\u0443. \u0412\
  \u0456\u043D \u043D\u0435\u0437\u0440\u0443\u0447\u043D\u0438\u0439, \u0456\u0437\
  \ \u043F\u043E\u0432\u0442\u043E\u0440\u044E\u0432\u0430\u043D\u0438\u043C \u043A\
  \u043E\u0434\u043E\u043C \u0456 \u0437\u0430 \u043D\u0438\u043C \u0432\u0430\u0436\
  \u043A\u043E \u0441\u043B\u0456\u0434\u043A\u0443\u0432\u0430\u0442\u0438."
lastmod: '2024-03-13T22:44:49.591975-06:00'
model: gpt-4-0125-preview
summary: "\u0420\u043E\u0437\u0433\u043B\u044F\u043D\u0435\u043C\u043E \u043F\u0440\
  \u043E\u0441\u0442\u0438\u0439 Bash \u0441\u043A\u0440\u0438\u043F\u0442, \u044F\
  \u043A\u0438\u0439 \u043F\u043E\u0442\u0440\u0435\u0431\u0443\u0454 \u0440\u0435\
  \u0444\u0430\u043A\u0442\u043E\u0440\u0438\u043D\u0433\u0443."
title: "\u0420\u0435\u0444\u0430\u043A\u0442\u043E\u0440\u0438\u043D\u0433"
weight: 19
---

## Як:
Розглянемо простий Bash скрипт, який потребує рефакторингу. Він незручний, із повторюваним кодом і за ним важко слідкувати:

```Bash
#!/bin/bash
echo "Введіть ім'я файлу:"
read filename
if [ -f "$filename" ]; then
    echo "Файл існує."
    count=$(grep -c "foo" "$filename")
    echo "Слово foo з'являється $count разів."
else
    echo "Файл не існує."
fi
```

Рефакторинг для підвищення ясності та можливості повторного використання може включати введення функцій і більш граціозне оброблення помилок:

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
    echo "Введіть ім'я файлу:"
    read -r filename
    echo "Введіть слово для пошуку:"
    read -r word

    if file_exists "$filename"; then
        count=$(count_occurrences "$word" "$filename")
        echo "Слово $word зустрічається $count разів."
    else
        echo "Файл не існує." >&2
        exit 1
    fi
}

main "$@"
```

Рефакторингова версія використовує функції для поліпшення читабельності та можливості потенційного повторного використання.

## Поглиблено:
Рефакторинг — це не концепція, яка з'явилася із Bash чи навіть високорівневих мов програмування; вона така ж стара, як і саме програмування. Термін був формалізований у книзі "Refactoring: Improving the Design of Existing Code" Мартіна Фаулера у 1999 році, зосереджуючись головним чином на об'єктно-орієнтованих мовах.

У контексті скриптів Bash рефакторинг часто означає розбиття довгих скриптів на функції, зменшення повторення за допомогою циклів або умовних конструкцій, а також уникнення поширених пасток, таких як неспроможність обробити пробіли у назвах файлів. Альтернативами Bash для скриптів, що стали надто складними, є Python або Perl, які пропонують кращі структури даних і обробку помилок для складних завдань.

Рефакторинг, специфічний для Bash, більше про дотримання кращих практик, таких як використання лапок для змінних, використання `[[ ]]` для тестів замість `[ ]`, і віддача переваги `printf` замість `echo` для надійного виводу. Деталі впровадження часто зводяться до дотримання посібників стилю та використання інструментів на кшталт `shellcheck` для статичного аналізу, щоб виявляти поширені помилки.

## Див. також:
- [Посібник зі стилю Shell від Google](https://google.github.io/styleguide/shellguide.html)
- [ShellCheck, інструмент статичного аналізу для скриптів оболонки](https://www.shellcheck.net/)
- [Мистецтво командного рядка](https://github.com/jlevy/the-art-of-command-line)
