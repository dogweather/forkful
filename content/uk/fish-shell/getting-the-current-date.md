---
title:                "Отримання поточної дати"
html_title:           "Bash: Отримання поточної дати"
simple_title:         "Отримання поточної дати"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Що і чому?

Отримання поточної дати - це процес визначення дати і часу на момент виконання коду. Праграмісти роблять це для налаштування таймерів, реєстрації подій чи для створення штампів часу для файлів.

## Як зробити:

Отримати поточну дату в Fish Shell можна з допомогою команди `date`.

```fish
~> date
Tue Sep 7 12:34:56 EEST 2021
```

Якщо вам потрібен більш деталізований вивід, використовуйте `%+`:

```fish
~> date "+%Y-%m-%d %H:%M:%S"
2021-09-07 12:34:56
```

## Поглиблений огляд:

Iсторико, команда `date` використовувалася на Unix-подібних системах з самого початку їх створення. Багато програм для роботи з датою і часом, наприклад `at` і `cron`, покладаються на `date` для отримання поточного часу. 

Альтернативи `date` включають використання `strftime`, який дозволяє більш гнучко налаштовувати вивід, але може бути менш зрозумілим для початківців.

В Fish Shell `date` імплементована з використанням системних викликів для отримання поточного часу з системного годинника, а потім форматує його до бажаного вигляду.

## Дивіться також:

- [Fish Shell документація за командою `date`](https://fishshell.com/docs/current/commands.html#date)
- [Стандарт POSIX для `strftime`](https://pubs.opengroup.org/onlinepubs/009695399/functions/strftime.html)
- [Unix `date` в мануалі](https://man7.org/linux/man-pages/man1/date.1.html)