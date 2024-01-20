---
title:                "Читання аргументів командного рядка"
html_title:           "Arduino: Читання аргументів командного рядка"
simple_title:         "Читання аргументів командного рядка"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Що це таке & навіщо?

Читання аргументів командного рядка обире їх з командного рядка при виконанні програми. Програмісти виконують це, щоб передати налаштування, параметри чи дані до своїх програм.

## Як це зробити:

Використовуйте змінну `ARGV`, яка є вмонтованою у Ruby і містить масив аргументів командного рядка.

```Ruby
puts "Надано аргументів: #{ARGV.length}"
ARGV.each_with_index do|v, i|
  puts "ARGV[#{i}] = #{v}"
end
```

Виконав цей код з аргументами "arg1 arg2 arg3", ви отримаєте на вихід:

```Ruby
Надано аргументів: 3
ARGV[0] = arg1
ARGV[1] = arg2
ARGV[2] = arg3
```

## Пориньмо глибше:

В Ruby, `ARGV` є основним способом отримати доступ до аргументів командного рядка, але це не завжди було так. У ранніх версіях Ruby було потрібно використовувати `getopts` або `OptionParser`.

Перевага використання `ARGV` є в тому, що воно просте і не потребує додаткових бібліотек. Але, якщо вам потрібна більша гнучкість і розширені можливості, ви можете використати бібліотеку `OptionParser`.

Як `ARGV`, так і `OptionParser` отримують аргументи командного рядка з середовища операційної системи. Однак `OptionParser` надає додаткові інструменти для аналізу введених даних.

## Дивіться також:


- Пост на StackOverflow про `ARGV` і `OptionParser`: [https://stackoverflow.com/questions/26434923/parse-command-line-arguments-in-a-ruby-script](https://stackoverflow.com/questions/26434923/parse-command-line-arguments-in-a-ruby-script)

- Детальний огляд `OptionParser`: [https://ruby-doc.org/stdlib-2.6.5/libdoc/optparse/rdoc/OptionParser.html](https://ruby-doc.org/stdlib-2.6.5/libdoc/optparse/rdoc/OptionParser.html)