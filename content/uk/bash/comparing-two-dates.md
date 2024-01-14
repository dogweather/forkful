---
title:                "Bash: Порівняння двох дат"
simple_title:         "Порівняння двох дат"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Чому

Компарація дат може бути корисна для багатьох сценаріїв, зокрема при роботі з даними відстеження часу, плануванні та подіях. Вона дозволяє швидко та ефективно порівнювати дати та виконувати різні дії на основі результатів порівняння.

## Як використовувати

Є кілька способів порівнювати дати в Bash. Один з них - використовувати команду `date -d` з ключем `+%s`, яка конвертує дату в формат UNIX timestamp. Наприклад:
```Bash
a="2021-01-01"
b="2021-01-02"
if [[ $(date -d "$a" +%s) -gt $(date -d "$b" +%s) ]]; then
    echo "$a is later than $b"
else
    echo "$b is later than $a"
fi
```
Виходом буде `2021-01-02 is later than 2021-01-01`.

Також можна використовувати вбудовані функції в Bash, такі як `date +%s`, для конвертації дат в timestamp. Наприклад:
```Bash
a="2021-01-01"
b="2021-01-02"
if (( $(date -d "$a" +%s) > $(date -d "$b" +%s) )); then
    echo "$a is later than $b"
else
    echo "$b is later than $a"
fi
```
Виходом буде те саме - `2021-01-02 is later than 2021-01-01`.

## Глибоке заглиблення

Використання команди `date -d` не дозволяє порівнювати дати з мілісекундами, тому у більш складних сценаріях краще використати вбудовані функції Bash. Також важливо враховувати формат дат, щоб уникнути помилок у порівнянні. Наприклад, дата у форматі `MM/DD/YYYY` може бути сприйнята як `DD/MM/YYYY`, що призведе до некоректного порівняння.

## Дивіться також

- [Офіційна документація Bash](https://www.gnu.org/software/bash/manual/bash.html)
- [10 Bash Programming Interview Questions for Linux Users](https://www.tecmint.com/linux-bash-scripting-interview-questions/)
- [Керівництво по використанню команди date в Bash](https://www.lifewire.com/bash-date-command-3572123)