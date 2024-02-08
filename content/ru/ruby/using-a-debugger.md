---
title:                "Использование отладчика"
aliases:
- ru/ruby/using-a-debugger.md
date:                  2024-01-29T00:03:32.370436-07:00
model:                 gpt-4-0125-preview
simple_title:         "Использование отладчика"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/ruby/using-a-debugger.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Использование отладчика в Ruby дает программистам сверхспособность останавливать свой код, инспектировать переменные и пошагово просматривать свой код. Люди делают это для исправления ошибок, понимания потока кода и чтобы точно видеть, что делают их написанные заклинания (код), когда происходит магия — или нет.

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
