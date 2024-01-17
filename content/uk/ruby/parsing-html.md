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

## Що і чому?

Розбирання HTML - це процес витягування інформації з HTML коду. Програмісти користуються цим для отримання даних з веб-сторінок, таких як ціни на товари, ім'я та контактна інформація людей, заголовки статей тощо.

## Як це зробити:

```Ruby
require 'nokogiri'
require 'open-uri'

# витягнути заголовок з веб-сторінки
doc = Nokogiri::HTML(URI.open('https://www.example.com'))
title = doc.css('h1').text
puts title

# витягнути всі посилання на сторінці
links = doc.css('a')
links.each do |link|
  puts link[:href]
end
```

### Приклад вихідного коду:

```HTML
<h1>Прикладний Заголовок</h1>

<a href="https://www.example.com">Прикладне посилання</a>
<a href="https://www.example.com/2">Ще одне посилання</a>
<a href="https://www.example.com/3">Третє посилання</a>
```

### Вихід програми:

```
Прикладний Заголовок
https://www.example.com
https://www.example.com/2
https://www.example.com/3
```

## Глибші зауваження:

Parsing HTML - це процес, який існує з самого початку веб-розвитку. Раніше ручна обробка HTML була єдиним способом отримати дані з веб-сторінок. Але з появою спеціальних бібліотек, таких як nokogiri, стало можливим автоматизувати цей процес.

У Ruby є також інші бібліотеки для парсингу HTML, такі як Mechanize та Hpricot. Вибір бібліотеки залежить від ваших потреб та зручності роботи з ними.

### Розбір алгоритму:

1. Отримати вихідний код HTML сторінки.
2. Використати бібліотеку для парсингу.
3. Запросити список елементів з конкретними тегами за допомогою CSS селекторів.
4. Обробити отриману інформацію згідно з потребами.

## Дивіться також:

- [Nokogiri документація](https://nokogiri.org/)
- [Розділ про парсинг HTML в Ruby на RubyLearning](http://rubylearning.com/satishtalim/ruby_html_parsing.html)
- [Відео урок про парсинг з nokogiri](https://www.youtube.com/watch?v=BAJorPirpOs)