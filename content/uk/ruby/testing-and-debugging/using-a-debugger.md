---
date: 2024-01-26 04:10:24.769979-07:00
description: "Ruby \u043F\u043E\u0441\u0442\u0430\u0432\u043B\u044F\u0454\u0442\u044C\
  \u0441\u044F \u0437 \u0432\u0431\u0443\u0434\u043E\u0432\u0430\u043D\u0438\u043C\
  \ \u0434\u0435\u0431\u0430\u0433\u0435\u0440\u043E\u043C, \u044F\u043A\u0438\u0439\
  \ \u043D\u0430\u0437\u0438\u0432\u0430\u0454\u0442\u044C\u0441\u044F `byebug`. \u0421\
  \u043F\u043E\u0447\u0430\u0442\u043A\u0443, \u0432\u043A\u043B\u044E\u0447\u0456\
  \u0442\u044C `byebug` \u0434\u043E \u0432\u0430\u0448\u043E\u0433\u043E Gemfile\
  \ \u0456 \u0437\u0430\u043F\u0443\u0441\u0442\u0456\u0442\u044C `bundle install`.\
  \ \u041F\u043E\u0442\u0456\u043C,\u2026"
lastmod: '2024-03-13T22:44:50.235113-06:00'
model: gpt-4-0125-preview
summary: "Ruby \u043F\u043E\u0441\u0442\u0430\u0432\u043B\u044F\u0454\u0442\u044C\u0441\
  \u044F \u0437 \u0432\u0431\u0443\u0434\u043E\u0432\u0430\u043D\u0438\u043C \u0434\
  \u0435\u0431\u0430\u0433\u0435\u0440\u043E\u043C, \u044F\u043A\u0438\u0439 \u043D\
  \u0430\u0437\u0438\u0432\u0430\u0454\u0442\u044C\u0441\u044F `byebug`. \u0421\u043F\
  \u043E\u0447\u0430\u0442\u043A\u0443, \u0432\u043A\u043B\u044E\u0447\u0456\u0442\
  \u044C `byebug` \u0434\u043E \u0432\u0430\u0448\u043E\u0433\u043E Gemfile \u0456\
  \ \u0437\u0430\u043F\u0443\u0441\u0442\u0456\u0442\u044C `bundle install`. \u041F\
  \u043E\u0442\u0456\u043C,\u2026"
title: "\u0412\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u043D\u043D\u044F \u0434\
  \u0435\u0431\u0430\u0433\u0435\u0440\u0430"
weight: 35
---

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
