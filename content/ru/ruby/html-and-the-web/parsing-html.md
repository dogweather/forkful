---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:00:16.868192-07:00
description: "\u0420\u0430\u0437\u0431\u043E\u0440 HTML \u043F\u0440\u0435\u0434\u0441\
  \u0442\u0430\u0432\u043B\u044F\u0435\u0442 \u0441\u043E\u0431\u043E\u0439 \u043F\
  \u0440\u043E\u0446\u0435\u0441\u0441 \u0430\u043D\u0430\u043B\u0438\u0437\u0430\
  \ \u0431\u043B\u043E\u043A\u0430 \u043A\u043E\u0434\u0430 HTML \u0434\u043B\u044F\
  \ \u043F\u043E\u043D\u0438\u043C\u0430\u043D\u0438\u044F \u0435\u0433\u043E \u0441\
  \u0442\u0440\u0443\u043A\u0442\u0443\u0440\u044B \u0438 \u0441\u043E\u0434\u0435\
  \u0440\u0436\u0430\u043D\u0438\u044F. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\
  \u043C\u0438\u0441\u0442\u044B \u0434\u0435\u043B\u0430\u044E\u0442 \u044D\u0442\
  \u043E \u0434\u043B\u044F \u0438\u0437\u0432\u043B\u0435\u0447\u0435\u043D\u0438\
  \u044F \u0434\u0430\u043D\u043D\u044B\u0445,\u2026"
lastmod: '2024-03-13T22:44:45.993569-06:00'
model: gpt-4-0125-preview
summary: "\u0420\u0430\u0437\u0431\u043E\u0440 HTML \u043F\u0440\u0435\u0434\u0441\
  \u0442\u0430\u0432\u043B\u044F\u0435\u0442 \u0441\u043E\u0431\u043E\u0439 \u043F\
  \u0440\u043E\u0446\u0435\u0441\u0441 \u0430\u043D\u0430\u043B\u0438\u0437\u0430\
  \ \u0431\u043B\u043E\u043A\u0430 \u043A\u043E\u0434\u0430 HTML \u0434\u043B\u044F\
  \ \u043F\u043E\u043D\u0438\u043C\u0430\u043D\u0438\u044F \u0435\u0433\u043E \u0441\
  \u0442\u0440\u0443\u043A\u0442\u0443\u0440\u044B \u0438 \u0441\u043E\u0434\u0435\
  \u0440\u0436\u0430\u043D\u0438\u044F."
title: "\u0420\u0430\u0437\u0431\u043E\u0440 HTML"
weight: 43
---

## Как это сделать:
Чтобы разобрать HTML в Ruby, установите гем 'Nokogiri' с помощью `gem install nokogiri`. Nokogiri похож на швейцарский армейский нож для работы с HTML и XML в Ruby. Вот быстрый пример:

```ruby
require 'nokogiri'
require 'open-uri'

# Загрузка содержимого HTML с веб-сайта
html_content = URI.open('http://example.com').read

# Разбор HTML
doc = Nokogiri::HTML(html_content)

# Извлечение заголовка
title = doc.xpath('//title').text
puts "Заголовок страницы: #{title}"
```

Это выведет что-то вроде: `Заголовок страницы: Example Domain`.

## Глубокое погружение
В ранние дни Ruby варианты для разбора HTML были ограничены. Встроенный REXML был медленным. Потом появился Hpricot, но он исчез. Nokogiri дебютировал в 2008 году, сочетая в себе простоту Hpricot с скоростью и мощью libxml, проверенного инструментария XML.

В мире разбора всегда есть альтернативы. Некоторые предпочитают встроенную библиотеку 'rexml' или 'oga', другой парсер XML/HTML для Ruby. Но Nokogiri остается фаворитом за его надежность и скорость, не говоря уже о широком ассортименте функций.

Под капотом Nokogiri преобразует HTML в модель объекта документа (DOM) — структуру дерева. Это упрощает навигацию и манипуляцию элементами. Используя XPath и CSS-селекторы, вы можете точно найти любую необходимую информацию.

## Смотрите также
- Гем Nokogiri: [https://nokogiri.org/](https://nokogiri.org/)
- Документация Ruby's rexml: [https://ruby-doc.org/stdlib-2.6.3/libdoc/rexml/rdoc/REXML/Document.html](https://ruby-doc.org/stdlib-2.6.3/libdoc/rexml/rdoc/REXML/Document.html)
- Альтернативный парсер 'oga': [https://github.com/YorickPeterse/oga](https://github.com/YorickPeterse/oga)
- Узнайте о XPath: [https://www.w3schools.com/xml/xpath_intro.asp](https://www.w3schools.com/xml/xpath_intro.asp)
