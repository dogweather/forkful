---
title:                "Друк відлагоджувального виводу"
html_title:           "Arduino: Друк відлагоджувального виводу"
simple_title:         "Друк відлагоджувального виводу"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

## Що і навіщо?
Друк відлагоджувального виводу (debug output) - це метод, який дозволяє програмістам відслідковувати стан та роботу їх програм. Ми використовуємо його для знаходження помилок у коді та зрозуміння, як він працює в реальний час.

## Як це зробити:
Для друку повідомлень для відлагодження у Ruby ми можемо використовувати метод `puts` або `print`.

```Ruby
number = 5
puts "Значення number: #{number}"
```

Програма вище виведе:

`Значення number: 5`

## Поглиблено:
Використання друку для відлагодження має довгу історію і яє головним методом відлагодження програм для багатьох розробників. Альтернативою може бути використання спеціалізованих інструментів для відлагодження, таких як `debugger` в Ruby. За допомогою цього инструменту ви можете зупиняти виконання вашої програми на певній точці і дослідити стан програми.

```Ruby
require 'debugger' 
number = 5 
debugger 
puts "Значення number: #{number}"
```

Даний код дозволить вам зупинити виконання програми перед викликом `puts` та освідомитись про стан змінних в цей момент.

## Дивись також:
1. [Розробка програмного забезпечення в Wikipedia](https://uk.wikipedia.org/wiki/%D0%A0%D0%BE%D0%B7%D1%80%D0%BE%D0%B1%D0%BA%D0%B0_%D0%BF%D1%80%D0%BE%D0%B3%D1%80%D0%B0%D0%BC%D0%BD%D0%BE%D0%B3%D0%BE_%D0%B7%D0%B0%D0%B1%D0%B5%D0%B7%D0%BF%D0%B5%D1%87%D0%B5%D0%BD%D0%BD%D1%8F)
2. [Відлагодження в Ruby](https://www.rubyguides.com/2015/06/ruby-debugging/)
3. [Ruby Documentation: Debugger](https://ruby-doc.org/stdlib-2.5.1/libdoc/debugger/rdoc/DEBUGGER__.html)