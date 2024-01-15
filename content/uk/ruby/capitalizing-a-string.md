---
title:                "Капіталізація рядка"
html_title:           "Ruby: Капіталізація рядка"
simple_title:         "Капіталізація рядка"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Чому

Залежно від ситуації, може бути багато причин для капіталізації рядка в Ruby. Наприклад, якщо ви хочете, щоб ваш код був більш зрозумілим для інших розробників, або якщо ви працюєте зі строками, отриманими зовнішніми джерелами і хочете перевести їх до заголовків або великих літер.

## Як

```ruby
text = "hello, world!"
puts text.capitalize  #=> "Hello, world!"
```

Це простий приклад коду, який капіталізує рядок "hello, world!". Можна також використовувати метод `upcase` для перетворення рядка великими літерами і `downcase` для зниження написання.

## Глибше поглибитися

Якщо ви хочете краще зрозуміти, як працює капіталізація в Ruby, варто розглянути її детальніше. Коли ми використовуємо метод `capitalize`, перша літера рядка стає великою, а решта залишається такою ж.

Наприклад, якщо ми маємо строку "hello world", після застосування методу `capitalize` ми отримаємо "Hello world". Однак, якщо ми маємо строку "hello, world!", після застосування методу `capitalize` ми отримаємо "Hello, world!". Це тому, що метод капіталізує тільки першу літеру рядка, і не впливає на решту символів.

## Дивіться також

- [Ruby документація про метод `capitalize`](https://docs.ruby-lang.org/en/master/String.html#method-i-capitalize)
- [Простий посібник із початку роботи з Ruby](https://www.ruby-lang.org/uk/documentation/quickstart)