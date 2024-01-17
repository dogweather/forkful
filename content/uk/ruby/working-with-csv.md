---
title:                "Робота з CSV"
html_title:           "Ruby: Робота з CSV"
simple_title:         "Робота з CSV"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/working-with-csv.md"
---

{{< edit_this_page >}}

# Що це таке та чому це потрібно?
Цей артикль розповість про модуль Ruby CSV, який дозволяє роботу з CSV файлами (Comma-Separated Values). Цей модуль стає дуже корисним для програмістів, оскільки CSV є одним із найпоширеніших форматів даних для обміну інформацією між різними додатками та системами.

# Як користуватися:
Нижче наведено приклад коду та виводу для додавання нових даних в CSV файл та читання вже наявних даних з нього, використовуючи модуль CSV.

```Ruby
require 'csv'

# додавання нової рядки даних до файлу
CSV.open("data.csv", "a") do |csv|
  csv << ["Новий запис", "Інформація", "Для збереження"]
end

# читання вже наявних даних з файлу
CSV.foreach("data.csv") do |row|
  puts row
end
```

Вивід:
Новий запис, Інформація, Для збереження

# Вдавайтеся вглиб:
Модуль CSV був випущений разом з Ruby 1.0 у 1995 році. Незабаром він став дуже популярним серед програмістів, оскільки дозволяє просто та швидко обробляти дані у форматі CSV. Альтернативами для роботи з CSV у Ruby є модулі SmarterCSV і FasterCSV, але вони в реальності є лише обгортками навколо модуля CSV.

В модулі CSV є також можливість налаштувати символи, що використовуються для розділення та обрамлення полів, що робить його дуже гнучким для роботи з різними форматами CSV файлів.

# Див. також:
- [Документація модуля CSV у Ruby](https://ruby-doc.org/stdlib-2.6.6/libdoc/csv/rdoc/CSV.html)
- [Документація модуля SmarterCSV у Ruby](https://rubygems.org/gems/smarter_csv)
- [Документація модуля FasterCSV у Ruby](https://github.com/JEG2/faster_csv)