---
title:                "Аналіз HTML"
aliases:
- uk/ruby/parsing-html.md
date:                  2024-02-03T19:13:24.351264-07:00
model:                 gpt-4-0125-preview
simple_title:         "Аналіз HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?
Парсинг HTML означає розбирання шматка коду HTML для розуміння його структури та змісту. Програмісти роблять це, щоб екстрагувати дані, маніпулювати змістом або мігрувати інформацію між форматами та системами.

## Як це зробити:
Щоб парсити HTML у Ruby, встановіть гем 'Nokogiri' за допомогою команди `gem install nokogiri`. Nokogiri - це як швейцарський нож для роботи з HTML і XML у Ruby. Ось швидкий приклад:

```ruby
require 'nokogiri'
require 'open-uri'

# Завантаження вмісту HTML з веб-сайту
html_content = URI.open('http://example.com').read

# Парсинг HTML
doc = Nokogiri::HTML(html_content)

# Екстракція заголовку
title = doc.xpath('//title').text
puts "Заголовок сторінки: #{title}"
```

Це виведе щось на кшталт: `Заголовок сторінки: Example Domain`.

## Поглиблено
На зорі Ruby, варіанти парсингу HTML були обмежені. REXML був вбудований, але повільний. Потім з'явився Hpricot, але його час швидко минув. Nokogiri з'явився у 2008 році, поєднуючи в собі простоту Hpricot зі швидкістю та потужністю libxml, перевіреного інструментарію для роботи з XML.

У світі парсингу завжди є альтернативи. Деякі віддають перевагу вбудованій бібліотеці 'rexml' або 'oga', іншому парсеру XML/HTML для Ruby. Але Nokogiri залишається фаворитом за його надійність і швидкість, не кажучи вже про величезний набір функцій.

Під капотом, Nokogiri перетворює HTML у Document Object Model (DOM) - структуру у вигляді дерева. Це робить його легким для навігації та маніпуляції елементами. Використовуючи XPath та CSS селектори, ви можете виокремити будь-яку інформацію, яка вам потрібна.

## Дивіться також
- Гем Nokogiri: [https://nokogiri.org/](https://nokogiri.org/)
- Документація Ruby's rexml: [https://ruby-doc.org/stdlib-2.6.3/libdoc/rexml/rdoc/REXML/Document.html](https://ruby-doc.org/stdlib-2.6.3/libdoc/rexml/rdoc/REXML/Document.html)
- Альтернативний парсер 'oga': [https://github.com/YorickPeterse/oga](https://github.com/YorickPeterse/oga)
- Дізнайтеся про XPath: [https://www.w3schools.com/xml/xpath_intro.asp](https://www.w3schools.com/xml/xpath_intro.asp)
