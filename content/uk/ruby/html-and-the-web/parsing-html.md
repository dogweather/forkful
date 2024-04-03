---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:24.351264-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0429\u043E\u0431 \u043F\u0430\u0440\u0441\u0438\u0442\u0438 HTML \u0443 Ruby,\
  \ \u0432\u0441\u0442\u0430\u043D\u043E\u0432\u0456\u0442\u044C \u0433\u0435\u043C\
  \ 'Nokogiri' \u0437\u0430 \u0434\u043E\u043F\u043E\u043C\u043E\u0433\u043E\u044E\
  \ \u043A\u043E\u043C\u0430\u043D\u0434\u0438 `gem install nokogiri`. Nokogiri -\
  \ \u0446\u0435 \u044F\u043A \u0448\u0432\u0435\u0439\u0446\u0430\u0440\u0441\u044C\
  \u043A\u0438\u0439 \u043D\u043E\u0436 \u0434\u043B\u044F\u2026"
lastmod: '2024-03-13T22:44:50.223972-06:00'
model: gpt-4-0125-preview
summary: "\u0429\u043E\u0431 \u043F\u0430\u0440\u0441\u0438\u0442\u0438 HTML \u0443\
  \ Ruby, \u0432\u0441\u0442\u0430\u043D\u043E\u0432\u0456\u0442\u044C \u0433\u0435\
  \u043C 'Nokogiri' \u0437\u0430 \u0434\u043E\u043F\u043E\u043C\u043E\u0433\u043E\u044E\
  \ \u043A\u043E\u043C\u0430\u043D\u0434\u0438 `gem install nokogiri`."
title: "\u0410\u043D\u0430\u043B\u0456\u0437 HTML"
weight: 43
---

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
