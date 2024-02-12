---
title:                "Склеивание строк"
date:                  2024-01-28T23:57:03.528927-07:00
model:                 gpt-4-0125-preview
simple_title:         "Склеивание строк"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/ruby/concatenating-strings.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Конкатенация строк – это всего лишь утонченный способ сказать "склеить их вместе конец к концу". Программисты делают это для объединения слов и предложений, для создания сообщений или динамической вставки значений в текст.

## Как:
В Ruby можно конкатенировать строки с помощью оператора `+` или метода `<<`, который изменяет строку на месте. Вот как соединить точки — или скорее, слова:

```Ruby
# Использование оператора +, который возвращает новую строку
greeting = "Hello, " + "world!"
puts greeting # Вывод: Hello, world!

# Использование метода <<, который изменяет оригинальную строку
name = "Alice"
name << ", познакомься с Bob"
puts name # Вывод: Alice, познакомься с Bob
```

## Погружение
Конкатенация существует в Ruby с момента его создания. Но со временем язык предоставил больше способов для сцепления строк.

Мы рассмотрели `+` и `<<`, но есть также метод `concat` и интерполяция.

- Использование `concat`: Этот метод похож на `<<`, но позволяет добавлять сразу несколько строк.
```Ruby
phrase = "Розы красные"
phrase.concat(", фиалки синие")
puts phrase # Вывод: Розы красные, фиалки синие
```

- Интерполяция: Вставляет переменные в строку без прямой конкатенации. Это аккуратнее и предпочтительнее для вставки переменных:
```Ruby
mood = "взволнован"
message = "Я #{mood} изучать Ruby!"
puts message # Вывод: Я взволнован изучать Ruby!
```

Интерполяция автоматически вызывает `to_s` для любой переменной, гарантируя, что типы данных, не являющиеся строками, корректно функционируют внутри строки.

Также помните, что дело не только в соединении слов; Ruby также следит за производительностью. Когда вы используете `+`, Ruby создает новую строку. Со временем или в циклах это может быстро заполнить память. В отличие от этого, `<<` и `concat` модифицируют оригинальную строку, что зачастую более эффективно.

## Смотрите также
- Документация по строкам Ruby: https://ruby-doc.org/core-3.1.2/String.html
- Статья об интерполяции строк в Ruby: https://www.rubyguides.com/2018/11/ruby-string-interpolation/
- Руководство по операторам Ruby: https://www.tutorialspoint.com/ruby/ruby_operators.htm