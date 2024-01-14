---
title:                "Ruby: Створення тимчасового файлу"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Чому

Створення тимчасового файлу є важливою функцією для будь-якого програміста, який працює з розробкою програм на Ruby. Це допомагає зберегти місце для тимчасових даних та уникнути збоїв в роботі програми.

## Як це зробити

Створення тимчасового файлу на Ruby дуже просте. Використовуючи клас `Tempfile`, ви можете створити тимчасовий файл за допомогою всього декількох рядочків коду. Давайте подивимося на приклад:

```Ruby
require 'tempfile'

# Створення тимчасового файлу
temp_file = Tempfile.new('myfile')

# Записуємо дані у файл
temp_file.write('Це мій тимчасовий файл!')

# Читаємо дані з файлу
puts temp_file.read

# Закриваємо файл
temp_file.close

# Видаляємо тимчасовий файл
temp_file.unlink
```

Результатом виконання цього коду буде виведення рядка `Це мій тимчасовий файл!` в консоль.

## Глибоке дослідження

Клас `Tempfile` насправді використовується для створення запасного файлу, який автоматично видаляється після того, як програма закриває файл. Крім того, його можна використовувати для створення багатьох тимчасових файлів та для використання їх у циклах.

Щоб дізнатися більше про `Tempfile`, ви можете переглянути [документацію](https://ruby-doc.org/stdlib-2.6.3/libdoc/tempfile/rdoc/Tempfile.html) або почитати цю [статтю](https://medium.com/@aguynamedryan/creating-temporary-files-and-directories-in-ruby-5c3d5f87a88a), що детально розглядає різні способи використання тимчасових файлів у Ruby.

## Дивись також

- [Документація класу Tempfile](https://ruby-doc.org/stdlib-2.6.3/libdoc/tempfile/rdoc/Tempfile.html)
- [Стаття про використання тимчасових файлів у Ruby](https://medium.com/@aguynamedryan/creating-temporary-files-and-directories-in-ruby-5c3d5f87a88a)