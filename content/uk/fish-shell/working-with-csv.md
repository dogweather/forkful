---
title:                "Робота з CSV файлами"
date:                  2024-01-19
html_title:           "Arduino: Робота з CSV файлами"
simple_title:         "Робота з CSV файлами"

category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/working-with-csv.md"
---

{{< edit_this_page >}}

## Що та Чому?

CSV - це формат файлів, який використовується для збереження табличних даних. Програмісти працюють з CSV, бо це простий та легкодоступний спосіб обміну даними між різними системами.

## Як це зробити:

Обробка CSV у Fish Shell може бути здійснена за допомогою командного рядка та скриптів:

```Fish Shell
# Читання CSV рядків
cat data.csv | while read -la line
    echo $line
end

# Розділення CSV рядків на поля та виведення одного поля (напр., друге поле)
cat data.csv | while read -la line
    set -l fields (string split "," -- $line[1])
    echo $fields[2]
end
```

Цей код виведе кожен рядок з файлу data.csv, а потім друге поле з кожного рядка.

## Поглиблений Розгляд

CSV (Comma-Separated Values) – ознака таблиці, де дані розділені комами, з'явилася ще у 1970-х. Є альтернативи як JSON чи XML, але CSV залишається популярним через свою простоту. В Fish Shell розділення полів здійснюється за допомогою `string split`, а ітерація рядків - через комбінацію `read` і циклу `while`.

## Також Дивіться

- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [String Manipulation in Fish](https://fishshell.com/docs/current/cmds/string.html)
