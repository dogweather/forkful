---
title:                "Аналіз дати з рядка"
date:                  2024-01-20T15:36:01.367000-07:00
html_title:           "Arduino: Аналіз дати з рядка"
simple_title:         "Аналіз дати з рядка"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Що та Чому?

Розбір дати зі строки — це процес перетворення текстового представлення дати в структурований формат, яким легко маніпулювати. Програмісти роблять це, щоби працювати з датами: порівнювати, додавати час, змінювати формати.

## Як це зробити:

Простий приклад у Fish Shell, щоби перетворити строку в дату:

```Fish Shell
set date_string "2023-03-23"
set epoch_time (date --date=$date_string +%s)
echo $epoch_time
```

Цей код повертає Unix час (кількість секунд з 1 січня 1970) у вигляді числа.

Для виводу в нормальному форматі можемо зробити так:

```Fish Shell
set normal_time (date --date=$date_string +"%Y-%m-%d %H:%M:%S")
echo $normal_time
```

Тут ми отримали строку типу "2023-03-23 00:00:00".

## Поглиблено:

Розбір дати зі строки — стандартний процес в будь-якій мові програмування. Головна мета — зробити дату зручною для обчислень та форматувань. У минулому кожен розробник писав свої функції, тепер це вбудовані функції або бібліотеки. У Fish Shell `date` — це зовнішня команда Unix, що працює з датами. Інші мови мають свої інструменти, наприклад, `datetime` у Python. Особливістю Fish Shell є те, що багато зручностей є "з коробки", а робота зі строками і датами не виключення.

## Також гляньте:

- Fish Shell Documentation: https://fishshell.com/docs/current/index.html
- Unix `date` command: https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
- POSIX standards for date and time: https://pubs.opengroup.org/onlinepubs/9699919799/utilities/date.html
- Stack Overflow, examples and community wisdom: https://stackoverflow.com/questions/tagged/fish
