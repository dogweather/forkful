---
title:                "Ruby: Створення тимчасового файлу"
simple_title:         "Створення тимчасового файлу"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Чому

Створення тимчасового файлу є важливим аспектом програмування Ruby. Він допомагає багатьом розробникам управляти даними, які потрібні лише під час виконання програми, а потім автоматично видаляти їх для економії простору та ресурсів.

## Як це зробити

Для створення тимчасового файлу в Ruby, використовуйте клас "Tempfile". Ось приклад коду:

```Ruby
require 'tempfile'
temp_file = Tempfile.new("example_file")
temp_file.write("This is a temporary file.")
puts File.read(temp_file)
temp_file.close
```

У цьому прикладі ми використовуємо метод "write" для запису тексту до тимчасового файлу. Потім ми читаємо цей файл за допомогою методу "read" та виводимо його в консоль. Не забудьте закрити файл за допомогою методу "close".

## Глибше

При створенні тимчасового файлу, важливо враховувати безпеку та ефективність. Наприклад, ви можете вказати, що файл повинен бути видалений після того, як програма завершила роботу. Також, ви можете встановити час життя файлу або обмежити доступ до нього за допомогою дозволів.

## Дивіться також

- [Документація Ruby про клас Tempfile](https://ruby-doc.org/stdlib-2.6.3/libdoc/tempfile/rdoc/Tempfile.html)
- [Стаття про тимчасові файли в Ruby](https://www.rubyguides.com/2015/05/temporary-files-in-ruby/)
- [Приклади використання класу Tempfile](https://www.rubydoc.info/stdlib/tmpdir%2FTempfile)