---
title:                "Отримання поточної дати"
html_title:           "Bash: Отримання поточної дати"
simple_title:         "Отримання поточної дати"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Що і для чого?
Отримання поточної дати є звичайною задачею для програмістів. Вона складає основу планування, проведення обліку, реалізації функцій відстежування і багато іншого у вашому програмному забезпеченні.

## Як зробити:
Ruby надає декілька методів для отримання поточної дати. Ось як ви можете це зробити з використанням модулів `Date` і `Time`:

```Ruby
require 'date'

date = Date.today
puts date
```

Виходом буде поточна дата у форматі `YYYY-MM-DD`.

А ось як отримати поточну дату і час за допомогою модуля `Time`:

```Ruby
time = Time.new
puts time
```

Виходом буде поточна дата і час у форматі `YYYY-MM-DD HH:MM:SS`.

## Пірнемо глибше
Історично в Ruby різні модулі були створені для вирішення отримання дати та часу: `Time`, `Date` і `DateTime`. `Time` був частиною базової бібліотеки Ruby, а `Date` і `DateTime` були додані пізніше, з більшими можливостями, але при цьому менш швидкими.

Як альтернативу, можемо використати гем `ActiveSupport`:

```Ruby
require 'active_support/all'

date = DateTime.current
puts date
```

Це надасть нам поточну дату і час з врахуванням часового поясу.

## Дивіться також:
1. Загальний огляд класів Ruby для роботи з датами і часом: 
[doc.ruby-lang.org](https://doc.ruby-lang.org/en/2.7.0/syntax/literals_rdoc.html#label-Dates)
2. Документація щодо модуля Date:
[ruby-doc.org](https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/Date.html)
3. Документація щодо модуля Time:
[ruby-doc.org](https://ruby-doc.org/core-2.7.0/Time.html)
4. Гем ActiveSupport:
[rubygems.org](https://rubygems.org/gems/activesupport)