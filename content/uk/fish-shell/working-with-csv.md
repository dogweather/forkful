---
title:                "Робота з csv"
html_title:           "Fish Shell: Робота з csv"
simple_title:         "Робота з csv"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/working-with-csv.md"
---

{{< edit_this_page >}}

## Для чого: 

Написання скріпту для роботи з CSV дуже корисно, якщо вам потрібно обробити великі об'єми даних, налагодити взаємодію з іншими програмами або створити звітну таблицю.

## Як це зробити:

```fish
# Зчитування CSV файлу за допомогою 'read_csv'
set -gx data (read_csv -m, myfile.csv)

# Вибір певних стовпців за допомогою 'select'
set vals $data | select name, price

# Обробка даних за допомогою 'for' циклу
for val in $vals
    echo "Назва: $val[1] | Ціна: $val[2]"
end
```

Вихідний код буде виглядати наступним чином:
```
Назва: Ручка | Ціна: 10
Назва: Олівець | Ціна: 5
Назва: Маркер | Ціна: 15
```

## Роздібрусямо питання глибше:

CSV є скороченням для "Comma Separated Values" і використовується для представлення табличних даних у текстовому форматі. Інструмент `read_csv` дозволяє зчитати CSV файл у вигляді масиву даних, а оператор `select` дозволяє обрати певні стовпці цих даних для подальшої обробки. Вміння працювати з CSV може знадобитися в багатьох сценаріях, тому це корисний навик для вивчення.

## Дивіться також:

- [Fish Shell документація](https://fishshell.com/docs/)
- [Робота з масивами даних в Fish Shell](https://fishshell.com/docs/current/cmds/set.html)
- [Курс з основ програмування](https://www.codecademy.com/tracks/learn-how-to-code)