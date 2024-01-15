---
title:                "Отримання поточної дати"
html_title:           "Fish Shell: Отримання поточної дати"
simple_title:         "Отримання поточної дати"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Чому

Отримати поточну дату може бути корисним для багатьох програм і скриптів. Наприклад, її можна використовувати для створення унікальних файлових імен, реєстрації часу виконання коду або відображення поточної дати на екрані.

## Як

Для отримання поточної дати використовуйте вбудовану функцію `date` із аргументом `%Y-%m-%d %H:%M`:

```Fish Shell
date +%Y-%m-%d %H:%M
```

Цей приклад виведе поточну дату у форматі `YYYY-MM-DD HH:MM`, наприклад, `2021-10-25 12:30`.

## Глибоке занурення

Функція `date` виконує форматування дати і часу відповідно до заданого шаблону. Вона також може приймати різні аргументи для зміни поведінки. Наприклад, `%s` поверне час у форматі UNIX timestamp, а `%e` доде новий рядок після виводу. Для більш докладної інформації про доступні аргументи і форматування дати, перегляньте мануал команди `date` за допомогою команди `man date`.

## Дивіться також

- [Офіційна документація Fish Shell (українською)](https://fishshell.com/docs/current/index.html)
- [Приклади вбудованих функцій в Fish Shell (англійською)](https://github.com/fish-shell/fish-shell/blob/master/doc_src/builtin_functions.rst)
- [Основні команди Unix (українською)](https://www.howto.com/?page_id=98&lang=uk)