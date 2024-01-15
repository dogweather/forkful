---
title:                "Розбір html"
html_title:           "Ruby: Розбір html"
simple_title:         "Розбір html"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/parsing-html.md"
---

{{< edit_this_page >}}

## Чому
Розбір HTML є важливою навичкою для веб-розробників та дослідників даних. Використовуючи Ruby, ви можете легко та ефективно отримати інформацію з різних веб-сторінок для подальшого використання.

## Як
```Ruby
require 'nokogiri'
require 'open-uri'

html = open("http://www.example.com/") # відкриваємо веб-сторінку
doc = Nokogiri::HTML(html) # створюємо об'єкт Nokogiri для розбору
puts doc.css('title').text # виводимо заголовок сторінки
puts doc.css('p').first.text # виводимо перший абзац
```
Sample Output:
```
Example Domain
This domain is for use in illustrative examples in documents. You may use this
domain in literature without prior coordination or asking for permission.
```

## Глибший розгляд
Нажаль, не всі веб-сторінки мають структурований HTML код, який можна легко розібрати. Деякі сторінки можуть містити неправильну розмітку, скрипти, або навіть нестандартні елементи, що можуть ускладнити процес розбору. В таких випадках, вам може знадобитися використовувати додаткові інструменти, такі як регулярні вирази, для отримання потрібних даних.

## Дивіться також
- [Nokogiri офіційна документація](https://nokogiri.org/)
- [Вступ до розбору HTML з допомогою Ruby](https://www.tutorialspoint.com/ruby-on-rails/rails-and-html-parsing.htm)