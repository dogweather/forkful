---
title:                "Парсинг HTML"
date:                  2024-01-20T15:34:34.315152-07:00
html_title:           "Arduino: Парсинг HTML"
simple_title:         "Парсинг HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/parsing-html.md"
---

{{< edit_this_page >}}

## Що це таке & Чому?
Парсинг HTML – це процес аналізу HTML-коду для витягнування інформації. Програмісти це роблять, щоб маніпулювати, збирати, або змінювати дані в веб-документах.

## Як це робити:
```
Ruby
require 'nokogiri'
require 'open-uri'

# Завантаження HTML з веб-сайту
doc = Nokogiri::HTML(URI.open('https://example.com'))

# Пошук елемента за тегом
puts doc.at_css("title").text

# Виведення всіх посилань
doc.css('a').each do |link|
  puts link.content + ": " + link['href']
end
```
Вивід:
```
Home Page: https://example.com
About Us: https://example.com/about
Contact: https://example.com/contact
```

## Поглиблений аналіз:
Нокогірі (Nokogiri) – це бібліотека для Ruby, названа на честь японського терміна "ножівка", яка була створена для зручного парсингу XML та HTML. До її появи, альтернативами були мануальний розбір або використання регулярних виразів, що часто були неточними та ненадійними. Nokogiri використовує нативні парсери, як от libxml2, для точного аналізу вмісту та швидкої роботи.

## Дивіться також:
- Документація Nokogiri: [https://nokogiri.org](https://nokogiri.org)
- W3C справочник по HTML: [https://www.w3.org/TR/html52/](https://www.w3.org/TR/html52/)
