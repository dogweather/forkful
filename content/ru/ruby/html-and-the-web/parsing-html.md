---
title:                "Разбор HTML"
aliases:
- /ru/ruby/parsing-html.md
date:                  2024-01-29T00:00:16.868192-07:00
model:                 gpt-4-0125-preview
simple_title:         "Разбор HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/ruby/parsing-html.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и зачем?
Разбор HTML представляет собой процесс анализа блока кода HTML для понимания его структуры и содержания. Программисты делают это для извлечения данных, манипулирования содержимым или миграции информации между форматами и системами.

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
