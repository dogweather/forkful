---
title:                "Аналіз дати з рядка"
html_title:           "C++: Аналіз дати з рядка"
simple_title:         "Аналіз дати з рядка"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Що і чому?

Парсинг дати з рядка - це процес переведення рядка, що містить дату, в дату, що може бути використана програмами. Програмісти це роблять, щоб можна було працювати з датами, як з об'єктами, а не просто як з текстом.

## Як це працює:

У Fish Shell це виглядає так:

```fish
set -l date_string "2022-02-20"
set -l parsed_date (date -u -j -f %Y-%m-%d $date_string "+%s")
```

Тут ми створюємо змінну `date_string` із рядком дати, а потім переводимо цей рядок в дату за допомогою команди `date -u -j -f %Y-%m-%d $date_string "+%s"`.

## Поглиблений розгляд

Історично, парсинг дати був болючим процесом через розмаїтість форматів дати. Розуміння форматів потребує знання локалізації та культурних особливостей, що може бути викликом. 

Альтернативною можливістю для Fish Shell є використання вбудованих команд інших оболонок, таких як bash чи zsh, але це може призвести до збоїв у скриптах, якщо не врахувати всі можливі відмінності між оболонками. 

Details implementation-wise, Fish Shell uses the inbuilt `date` command with specified flags to convert the string into a timestamp. If you want to use a different format, alter the format specifier (like %Y-%m-%d) inside the date command. 

## Дивись також 

- [Офіційна документація Fish Shell про управління датою та часом](https://fishshell.com/docs/current/index.html#date-and-time)
- [Туторіал про обробку дати і часу в Unix shell](https://www.cyberciti.biz/faq/linux-unix-formatting-dates-for-display/)
- [StackOverflow розмови про парсинг дати в Fish Shell](https://stackoverflow.com/questions/24582338/how-can-i-get-beautiful-date-time-on-the-command-line-interface)