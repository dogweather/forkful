---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:33.728341-07:00
description: "\u041A\u0430\u043A: \u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u0438\
  \u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0435\u043C REXML, \u0432\u043A\u043B\
  \u044E\u0447\u0435\u043D\u043D\u044B\u0439 \u0432 Ruby, \u0434\u043B\u044F \u0440\
  \u0430\u0437\u0431\u043E\u0440\u0430 \u0444\u0440\u0430\u0433\u043C\u0435\u043D\u0442\
  \u0430 XML."
lastmod: '2024-03-13T22:44:46.041137-06:00'
model: gpt-4-0125-preview
summary: "\u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u0438\u0441\u043F\u043E\u043B\
  \u044C\u0437\u0443\u0435\u043C REXML, \u0432\u043A\u043B\u044E\u0447\u0435\u043D\
  \u043D\u044B\u0439 \u0432 Ruby, \u0434\u043B\u044F \u0440\u0430\u0437\u0431\u043E\
  \u0440\u0430 \u0444\u0440\u0430\u0433\u043C\u0435\u043D\u0442\u0430 XML."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 XML"
weight: 40
---

## Как:
Давайте используем REXML, включенный в Ruby, для разбора фрагмента XML:
```Ruby
require 'rexml/document'
include REXML

xml_data = <<-XML
<fruits>
  <fruit name="apple" color="green"/>
  <fruit name="banana" color="yellow"/>
</fruits>
XML

document = Document.new(xml_data)
document.elements.each('fruits/fruit') { |element|
  puts "Имя: #{element.attributes['name']}, Цвет: #{element.attributes['color']}"
}
```
Вывод:
```
Имя: apple, Цвет: green
Имя: banana, Цвет: yellow
```

Генерация XML также проста:
```Ruby
doc = Document.new
doc.add_element 'fruits'
apple = doc.root.add_element 'fruit', {'name' => 'apple', 'color' => 'green'}
banana = doc.root.add_element 'fruit', {'name' => 'banana', 'color' => 'yellow'}
puts doc
```
Вывод XML:
```XML
<fruits>
  <fruit name="apple" color="green"/>
  <fruit name="banana" color="yellow"/>
</fruits>
```

## Подробнее:
Корни XML уходят в 1990-е годы как упрощенный поднабор SGML для веб-документов. XML громоздкий, но высоко структурированный, и именно поэтому он до сих пор актуален. Это не единственный вариант - JSON и YAML стали популярными благодаря своей простоте - но XML остается востребованным во многих предприятиях и устаревших системах.

Ruby предлагает несколько способов работы с XML. REXML - это библиотека на чистом Ruby, которая легка в освоении. Nokogiri - это гем, который использует более быстрые библиотеки C, предлагая скорость и дополнительные функции. Что выбрать? Начните с REXML для небольших задач и переходите к Nokogiri, если вам нужна большая мощность.

Внутри, анализ XML сводится к преобразованию строк в модели DOM или SAX. DOM создает дерево в памяти, в то время как SAX стримит документ и инициирует события во время его разбора. REXML предлагает обе модели, но склонен быть медленнее, чем расширения C, такие как используемые Nokogiri.

## Смотрите также:
- Документация Ruby REXML: https://www.rubydoc.info/stdlib/rexml
- Гем Nokogiri: https://nokogiri.org/
- Спецификация XML: https://www.w3.org/XML/
- Введение в SAX: https://www.saxproject.org/
- Сравнение YAML vs. JSON vs. XML: https://www.upwork.com/resources/json-vs-xml-vs-yaml
