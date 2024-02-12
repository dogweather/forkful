---
title:                "Робота з CSV"
aliases:
- /uk/ruby/working-with-csv.md
date:                  2024-02-03T19:21:25.192586-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?

Робота з файлами CSV в Ruby забезпечує простий підхід для обробки табличних даних. Програмісти часто займаються цим для парсингу даних, їхньої вилучення, трансформації та зберігання, що робить цю навичку критично важливою для завдань, пов'язаних з маніпуляцією даними або аналізом.

## Як це зробити:

Ruby включає бібліотеку CSV за замовчуванням, що спрощує читання з файлів CSV та запис у них. Ось як ви можете використати це для загальних завдань:

### Читання з файлу CSV
Для читання з файлу CSV спочатку вам потрібна бібліотека CSV. Потім ви можете ітерувати по рядках або читати їх у масив.

```ruby
require 'csv'

# Читання кожного рядка як масиву
CSV.foreach("data.csv") do |row|
  puts row.inspect
end

# Вивід для кожного рядка може виглядати так: ["data1", "data2", "data3"]
```

### Запис в CSV
Запис в файл CSV також є прямолінійним. Ви можете додавати дані до існуючого файлу або створювати новий файл для запису.

```ruby
require 'csv'

CSV.open("output.csv", "wb") do |csv|
  csv << ["header1", "header2", "header3"]
  csv << ["value1", "value2", "value3"]
end

# Це створює або перезаписує 'output.csv' з заданими заголовками та значеннями.
```

### Парсинг рядка CSV
Іноді вам потрібно парсити дані CSV безпосередньо з рядка. Ось як:

```ruby
require 'csv'

data = "name,age,city\nJohn Doe,29,New York\nJane Doe,31,Chicago"
csv = CSV.parse(data, headers: true)

csv.each do |row|
  puts "#{row['name']} - #{row['age']} - #{row['city']}"
end

# Очікуваний вивід:
# John Doe - 29 - Нью-Йорк
# Jane Doe - 31 - Чикаго
```

### Використання SmarterCSV
Для складніших завдань з CSV можна використовувати гем `SmarterCSV`. Спочатку встановіть гем:

```shell
gem install smarter_csv
```

Потім ви можете використовувати його для роботи з великими файлами або для більш складного парсингу та маніпуляції:

```ruby
require 'smarter_csv'

options = {}
data = SmarterCSV.process('large_data.csv', options)

data.each do |hash|
  puts hash.inspect
end

# Це прочитає 'large_data.csv' і виведе кожен рядок як хеш на основі заголовків.
```

Підсумовуючи, вбудована бібліотека CSV в Ruby разом із сторонніми гемами, як-от `SmarterCSV`, забезпечує міцну підтримку для обробки даних CSV, дозволяючи ефективно виконувати завдання з обробки та маніпуляції даними.
