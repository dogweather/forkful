---
title:                "Виведення налагоджувальної інформації"
aliases:
- uk/ruby/printing-debug-output.md
date:                  2024-01-20T17:53:35.257739-07:00
model:                 gpt-4-1106-preview
simple_title:         "Виведення налагоджувальної інформації"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

## Що це таке та навіщо?
Debugging - це як детективна робота. Треба шукати хто, де, і коли наплутав у вашому коді. Виведення дебаг-інформації допомагає зрозуміти, що саме робить код і чому щось іде не так, як ми планували.

## Як це зробити:
```Ruby
# Простий вивід інформації до консолі
puts "Це повідомлення виводиться на екран"

# Щось пошло не так? Подивимось, що в змінній:
debug_variable = "Щось підозріле"
p debug_variable

# Хочете більше деталей? Використайте pp для красивого виводу:
require 'pp'
complex_variable = { a: 1, b: "text", c: [1, 2, 3] }
pp complex_variable
```

Простою мовою, замість загадок – чіткі відповіді прямо в терміналі.

## Пірнемо глибше:
Раніше, коли Ruby тільки з’являвся, не було стільки інструментів для дебагінгу. Виведення інформації у консоль було ключем до розуміння проблем. І досі це ефективний спосіб. Альтернативою є використання інструментів як IRB або Pry, де можна в реальному часі експериментувати з кодом. Деталі реалізації варіюються від простого `puts` до використання бібліотек, що дозволяють більше контролю, таких як `logger`.

## Дивіться також:
- [Ruby Doc - Kernel#puts](https://ruby-doc.org/core-3.1.0/Kernel.html#method-i-puts)
- [Ruby Doc - Kernel#p](https://ruby-doc.org/core-3.1.0/Kernel.html#method-i-p)
- [Ruby Doc - PP#pp](https://ruby-doc.org/stdlib-3.1.0/libdoc/pp/rdoc/PP.html#method-c-pp)
- [Pry documentation](https://github.com/pry/pry) - мощна альтернатива IRB для дебагінгу.
- [Logger Class](https://ruby-doc.org/stdlib-3.1.0/libdoc/logger/rdoc/Logger.html) - якщо вам потрібна більш гнучка система логування.
