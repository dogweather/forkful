---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:03:32.370436-07:00
description: "Ruby \u043F\u043E\u0441\u0442\u0430\u0432\u043B\u044F\u0435\u0442\u0441\
  \u044F \u0441 \u0432\u0441\u0442\u0440\u043E\u0435\u043D\u043D\u044B\u043C \u043E\
  \u0442\u043B\u0430\u0434\u0447\u0438\u043A\u043E\u043C, \u043A\u043E\u0442\u043E\
  \u0440\u044B\u0439 \u043D\u0430\u0437\u044B\u0432\u0430\u0435\u0442\u0441\u044F\
  \ `byebug`. \u0421\u043D\u0430\u0447\u0430\u043B\u0430 \u0432\u043A\u043B\u044E\u0447\
  \u0438\u0442\u0435 `byebug` \u0432 \u0432\u0430\u0448 Gemfile \u0438 \u0437\u0430\
  \u043F\u0443\u0441\u0442\u0438\u0442\u0435 `bundle install`. \u0417\u0430\u0442\u0435\
  \u043C,\u2026"
lastmod: '2024-03-13T22:44:46.005746-06:00'
model: gpt-4-0125-preview
summary: "Ruby \u043F\u043E\u0441\u0442\u0430\u0432\u043B\u044F\u0435\u0442\u0441\u044F\
  \ \u0441 \u0432\u0441\u0442\u0440\u043E\u0435\u043D\u043D\u044B\u043C \u043E\u0442\
  \u043B\u0430\u0434\u0447\u0438\u043A\u043E\u043C, \u043A\u043E\u0442\u043E\u0440\
  \u044B\u0439 \u043D\u0430\u0437\u044B\u0432\u0430\u0435\u0442\u0441\u044F `byebug`.\
  \ \u0421\u043D\u0430\u0447\u0430\u043B\u0430 \u0432\u043A\u043B\u044E\u0447\u0438\
  \u0442\u0435 `byebug` \u0432 \u0432\u0430\u0448 Gemfile \u0438 \u0437\u0430\u043F\
  \u0443\u0441\u0442\u0438\u0442\u0435 `bundle install`. \u0417\u0430\u0442\u0435\u043C\
  ,\u2026"
title: "\u0418\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u043D\u0438\u0435\
  \ \u043E\u0442\u043B\u0430\u0434\u0447\u0438\u043A\u0430"
weight: 35
---

## Как это сделать:
Ruby поставляется с встроенным отладчиком, который называется `byebug`. Сначала включите `byebug` в ваш Gemfile и запустите `bundle install`. Затем, разместите `byebug` там, где вы хотите, чтобы ваша программа сделала паузу.

```Ruby
require 'byebug'

def calculate_magic(number)
  byebug
  magic_number = number * 7
  return magic_number
end

puts calculate_magic(6)
```

При запуске этого скрипта выполнение остановится на `byebug`, и вы окажетесь в интерактивной сессии, где можно будет вводить команды вроде:

```
step
next
continue
var local
```

Пример вывода будет выглядеть так:

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

## Глубокое погружение:
Еще до появления `byebug`, рубиисты использовали `debugger` и `pry`. Последний, `pry`, это не просто отладчик; это мощная REPL, которая также может использоваться для отладки с точкой останова `binding.pry`.

Варианты к `byebug` от Ruby включают `pry-byebug`, который сочетает в себе функциональность `pry` и `byebug`, и `ruby-debug`, который является более старым гемом, не поддерживаемым активно.

Когда вы вызываете `byebug`, отладчик приостанавливает выполнение вашего кода и дает вам возможность заглянуть во время выполнения. Вы можете видеть и изменять переменные, переходить в разные точки кода, и даже выполнять некоторый код Ruby построчно. Это как будто иметь способность перемещаться во времени для вашего кода Ruby.

## Смотрите также:
- Репозиторий Byebug на GitHub: [https://github.com/deivid-rodriguez/byebug](https://github.com/deivid-rodriguez/byebug)
- Документация Pry: [https://github.com/pry/pry](https://github.com/pry/pry)
- Руководство по отладке приложений Rails: [https://guides.rubyonrails.org/debugging_rails_applications.html](https://guides.rubyonrails.org/debugging_rails_applications.html)
