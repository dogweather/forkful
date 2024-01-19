---
title:                "Розбір HTML"
html_title:           "Arduino: Розбір HTML"
simple_title:         "Розбір HTML"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/parsing-html.md"
---

{{< edit_this_page >}}

## Що й чому?
Парсинг HTML це процес вилучення потрібних даних з HTML-документа. Програмісти роблять це, щоб організувати великі обсяги web-інформації, яку потім можна використовувати за потреби.

## Як це робиться:
Використаємо Ruby-бібліотеку Nokogiri для парсингу HTML. 

```Ruby
require 'nokogiri'
require 'open-uri'

# Відкриваємо веб-сторінку
doc = Nokogiri::HTML(open("https://www.example.com"))

# Знайдемо всі посилання на сторінці
links = doc.css('a')

links.each { |link| puts link['href'] }
```

У наведеному вище прикладі ми отримаємо список всіх посилань, які є на сторінці example.com.

## На глибині:
1. Історичний контекст: Парсинг HTML був потужним інструментом ще зі стародавніх часів, коли 'веб-гребінки' за допомогою цього інструменту збирали дані з веб-сайтів.
2. Альтернативи: Крім Nokogiri, можна використовувати інші Ruby-бібліотеки, такі як Oga або Hpricot.
3. Деталі впровадження: Часто парсинг HTML використовується в комбінації з web scraping, коли даний метод дозволяє витягти конкретні елементи за допомогою селекторів CSS або XPath.

## Див. також:
- Офіційна документація Nokogiri: [https://nokogiri.org](https://nokogiri.org)
- Обговорення парсингу HTML на Stack Overflow: [https://stackoverflow.com/questions/tagged/html-parsing](https://stackoverflow.com/questions/tagged/html-parsing)
- Документація Ruby про обробку веб-даних: [https://ruby-doc.org/stdlib-2.6.1/libdoc/open-uri/rdoc/OpenURI/OpenRead.html](https://ruby-doc.org/stdlib-2.6.1/libdoc/open-uri/rdoc/OpenURI/OpenRead.html)