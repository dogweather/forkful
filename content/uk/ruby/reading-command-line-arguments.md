---
title:                "Читання аргументів командного рядка"
date:                  2024-01-20T17:56:55.141289-07:00
model:                 gpt-4-1106-preview
simple_title:         "Читання аргументів командного рядка"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Що це таке та навіщо?

Читання аргументів командного рядка – це процес, коли програми Ruby вибирають дані прямо з того, що ви вводите в терміналі. Програмісти це роблять, щоб зробити програми гнучкішими та налаштовуваними користувачами.

## Як це зробити:

Аргументи командного рядка доступні через глобальний масив `ARGV`. Приклад:

```Ruby
# script.rb
puts "Ви ввели: #{ARGV}"
```

Якщо ви запустите:
```
ruby script.rb Це тест
```

Отримаєте:
```
Ви ввели: ["Це", "тест"]
```

### Отримання конкретних аргументів:
```Ruby
# specific_arg.rb
puts "Перший аргумент: #{ARGV[0]}"
puts "Другий аргумент: #{ARGV[1]}"
```

Запуск:
```
ruby specific_arg.rb раз два
```

Результат:
```
Перший аргумент: раз
Другий аргумент: два
```

### Використання `OptionParser` для аргументів з іменами:
```Ruby
require 'optparse'

options = {}
OptionParser.new do |opts|
  opts.banner = "Використання: script.rb [опції]"

  opts.on("-n", "--name NAME", "Введіть ім'я") do |n|
    options[:name] = n
  end

  opts.on("-a", "--age AGE", "Введіть вік") do |a|
    options[:age] = a
  end
end.parse!

puts "Ім'я: #{options[:name]}"
puts "Вік: #{options[:age]}"
```

Запуск:
```
ruby script.rb --name "Олег" --age "35"
```

Результат:
```
Ім'я: Олег
Вік: 35
```

## Поглиблений розгляд:
Аргументи командного рядка - не новинка, вони існують з моменту появи перших операционних систем. У Ruby, `ARGV` - це простий спосіб отримання даних, але існують альтернативи. 

* `OptionParser` є більш потужним для іменованих аргументів та валідації;
* `ENV` містить змінні середовища, ящо ще один спосіб модифікації поведінки програми.
* `gets` дозволяє читати ввод з консолі після запуску програми.

Варто зберегти аргументи в змінну, якщо плануєте їх часто використовувати - це може зменшити можливі помилки і поліпшити читабельність.

## Подивитися також:

* [OptionParser](https://ruby-doc.org/stdlib-2.7.0/libdoc/optparse/rdoc/OptionParser.html) - документація по `OptionParser`.
* [Environment Variables in Ruby](https://www.rubyguides.com/2019/01/ruby-environment-variables/) - стаття з роз'ясненням роботи зі змінними середовища.
* [The ARGF Object](https://ruby-doc.org/core-2.7.0/ARGF.html) - документація по ARGF, об'єкт який представляє поєднання `ARGV` та `$stdin`.
