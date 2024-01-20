---
title:                "Аналіз дати з рядка"
html_title:           "C++: Аналіз дати з рядка"
simple_title:         "Аналіз дати з рядка"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/bash/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Що і чому?
Розбір дати з рядка - це процес видобування конкретних значень дати, таких як день, місяць, рік, з текстового рядка. Програмісти роблять це, щоб забезпечити можливість обробки і маніпуляції даними про дату в програмах.

## Як це зробити:
В Bash можна розпарсити дату з рядка за допомогою команди `date` і `--date` флагу. Наприклад:

```Bash
date_string="2022-06-14 10:34:56"
result_date=$(date --date="$date_string" '+%Y-%m-%d %H:%M:%S')
echo $result_date
```

Видалений рядок:
```
2022-06-14 10:34:56
```

## Поглиблений огляд:
Історично, парсинг дати був важливим аспектом комп'ютерних програм, особливо в базах даних і веб-застосунків. Бездоганна робота з датами та часом - ключ до успіху великої кількості технологій.

Є інші способи парсингу дати, наприклад, використання Perl або Python скриптів, але Bash є універсальним та ефективним для простих операцій.

Щодо деталей імплементації, Bash обробляє всі строки як є, без перетворень типів. Це означає, що рядки з датою мають бути у форматі, який може розуміти Bash.

## Що ще подивитись:
1. Офіційна документація Bash: [https://www.gnu.org/software/bash/manual/bash.html](https://www.gnu.org/software/bash/manual/bash.html)
2. Мануал про команду `date`: [https://www.man7.org/linux/man-pages/man1/date.1.html](https://www.man7.org/linux/man-pages/man1/date.1.html)