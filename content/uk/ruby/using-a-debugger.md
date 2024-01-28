---
title:                "Використання дебагера"
date:                  2024-01-26T04:10:24.769979-07:00
model:                 gpt-4-0125-preview
simple_title:         "Використання дебагера"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/using-a-debugger.md"
---

{{< edit_this_page >}}

## Що і Чому?

Використання дебагера в Ruby надає програмістам надздібність зупиняти свій код, інспектувати змінні та крок за кроком проходити свій код. Люди роблять це, щоб виправити помилки, зрозуміти потік виконання коду, та бачити, що саме робить їхній написаний заклин (код), коли магія відбувається—або ні.

## Як це зробити:

Ruby поставляється з вбудованим дебагером, який називається `byebug`. Спочатку, включіть `byebug` до вашого Gemfile і запустіть `bundle install`. Потім, розмістіть `byebug` там, де ви хочете, щоб ваша програма зробила паузу.

```Ruby
require 'byebug'

def calculate_magic(number)
  byebug
  magic_number = number * 7
  return magic_number
end

puts calculate_magic(6)
```

Запуск цього скрипта призупинить виконання на `byebug`, і ви будете кинуті в інтерактивну сесію, де ви можете вводити команди на кшталт:

```
step
next
continue
var local
```

Зразок виводу покаже вам запит наступного вигляду:

```
[2, 11] in example.rb
    2: 
    3: def calculate_magic(number)
    4:   byebug
=>  5:   magic_number = number * 7
    6:   return magic_number
    7: end
    8: 
    9: puts calculate_magic(6)
(byebug) 
```

## Поглиблено:

Далеко до появи `byebug`, рубісти використовували `debugger` та `pry`. Останній, `pry`, це більше ніж дебагер; це потужна REPL (Read-Eval-Print Loop), яку також можна використовувати для дебагінгу з точкою перериву `binding.pry`.

Альтернативи до `byebug` в Ruby включають `pry-byebug`, який поєднує `pry` з функціональністю `byebug`, і `ruby-debug`, який є старшим гемом, що вже не підтримується активно.

Коли ви викликаєте `byebug`, дебагер призупиняє виконання вашого коду та дозволяє вам зазирнути у рантайм. Ви можете бачити та змінювати змінні, переходити до різних точок у коді, і навіть крок за кроком виконувати деякі рядки коду Ruby. Це трохи як мати здібності подорожі в часі для вашого коду Ruby.

## Дивіться також:

- Репозиторій Byebug на GitHub: [https://github.com/deivid-rodriguez/byebug](https://github.com/deivid-rodriguez/byebug)
- Документація Pry: [https://github.com/pry/pry](https://github.com/pry/pry)
- Посібник з налагодження Rails додатків: [https://guides.rubyonrails.org/debugging_rails_applications.html](https://guides.rubyonrails.org/debugging_rails_applications.html)
