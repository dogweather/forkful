---
title:                "Работа с XML"
aliases:
- /ru/ruby/working-with-xml.md
date:                  2024-01-29T00:05:33.728341-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/ruby/working-with-xml.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?
Работа с XML означает анализ, генерацию и манипулирование документами XML (eXtensible Markup Language) с использованием кода. Программисты делают это для взаимодействия с множеством веб-сервисов, файлами конфигурации и форматами обмена данными, где XML является общепринятым языком.

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
