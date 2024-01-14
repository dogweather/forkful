---
title:                "Ruby: Друк відлагоджувального виводу"
programming_language: "Ruby"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

## Чому

В програмуванні важливо вміти дебажити (відлагоджувати) свій код, тобто знаходити та виправляти помилки. Для цього часто використовуються спеціальні функції, які допомагають виводити поточний стан програми. Одним з таких інструментів є виведення відладочної інформації на екран.

## Як виконати

Для виведення відладочної інформації використовуються методи `print` та `puts`. Розглянемо їх на прикладі:

```Ruby
def add(num1, num2)
  puts "Запускаємо функцію додавання..."
  puts "Перший аргумент: #{num1}"
  puts "Другий аргумент: #{num2}"
  result = num1 + num2
  puts "Результат: #{result}"
  result
end

add(5, 10)
```

У вище наведеному прикладі, ми виконуємо функцію додавання та виводимо відладочну інформацію за допомогою функцій `puts` та `print`. Такий підхід дозволяє бачити поточний стан програми та знаходити помилки під час виконання коду.

## Глибокий занурення

У декотрим випадках, встановлювати точки зупинки для відлагодження коду може бути неефективно та часозатратно. Тоді виведення відладочної інформації стає незамінним інструментом для аналізу роботи програми. У Ruby також є спеціальний модуль `debug`, який допомагає здійснювати виведення відладочної інформації в більш зручному форматі.

## Дивіться також

- [The Ruby Debugger](https://ruby.github.io/debugger/)
- [Debugging Ruby with Pry](https://medium.com/rubyinside/introduction-to-debugging-ruby-with-pry-36ed09ceb157)
- [Debugging Techniques for Ruby on Rails Applications](https://guides.rubyonrails.org/debugging_rails_applications.html)