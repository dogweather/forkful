---
date: 2024-01-26 04:35:32.058939-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u0432\u0438\u043A\u043E\u0440\u0438\
  \u0441\u0442\u0430\u0454\u043C\u043E REXML, \u0449\u043E \u0432\u0445\u043E\u0434\
  \u0438\u0442\u044C \u0434\u043E \u0441\u043A\u043B\u0430\u0434\u0443 Ruby, \u0434\
  \u043B\u044F \u043F\u0430\u0440\u0441\u0438\u043D\u0433\u0443 \u0444\u0440\u0430\
  \u0433\u043C\u0435\u043D\u0442\u0430 XML."
lastmod: '2024-03-13T22:44:50.268868-06:00'
model: gpt-4-0125-preview
summary: "\u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u0432\u0438\u043A\u043E\u0440\
  \u0438\u0441\u0442\u0430\u0454\u043C\u043E REXML, \u0449\u043E \u0432\u0445\u043E\
  \u0434\u0438\u0442\u044C \u0434\u043E \u0441\u043A\u043B\u0430\u0434\u0443 Ruby,\
  \ \u0434\u043B\u044F \u043F\u0430\u0440\u0441\u0438\u043D\u0433\u0443 \u0444\u0440\
  \u0430\u0433\u043C\u0435\u043D\u0442\u0430 XML."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 XML"
weight: 40
---

## Як це зробити:
Давайте використаємо REXML, що входить до складу Ruby, для парсингу фрагмента XML:
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
document.elements.each('fruits/fruit') do |element|
  puts "Name: #{element.attributes['name']}, Color: #{element.attributes['color']}"
end
```
Вивід:
```
Name: apple, Color: green
Name: banana, Color: yellow
```

Генерація XML також пряма:
```Ruby
doc = Document.new
doc.add_element 'fruits'
apple = doc.root.add_element 'fruit', {'name' => 'apple', 'color' => 'green'}
banana = doc.root.add_element 'fruit', {'name' => 'banana', 'color' => 'yellow'}
puts doc
```
XML вивід:
```XML
<fruits>
  <fruit name="apple" color="green"/>
  <fruit name="banana" color="yellow"/>
</fruits>
```

## Поглиблене вивчення:
Коріння XML сягає 1990-х років як спрощений підмножина SGML для веб-документів. Вона многослівна, але високо структурована, і саме тому вона залишилася. XML не єдиний варіант – JSON та YAML стали популярними завдяки своїй простоті – але XML сильно утримується в багатьох корпоративних і спадкових системах.

Ruby пропонує кілька способів роботи з XML. REXML – це бібліотека на чистому Ruby, яка легка для початку роботи. Nokogiri – це гем, що використовує більш швидкі C-бібліотеки, пропонуючи швидкість та додаткові можливості. Що вибрати? Почніть з REXML для менших завдань, і переходьте до Nokogiri, якщо вам потрібна більша потужність.

Під капотом, парсинг XML означає перетворення рядків на моделі DOM або SAX. DOM створює дерево в пам’яті, тоді як SAX потоково обробляє документ і викликає події, коли парсить. REXML пропонує обидві моделі, але, як правило, працює повільніше, ніж розширення на C, такі як ті, що використовує Nokogiri.

## Див. також:
- Документація Ruby REXML: https://www.rubydoc.info/stdlib/rexml
- Гем Nokogiri: https://nokogiri.org/
- Специфікація XML: https://www.w3.org/XML/
- Вступ до SAX: https://www.saxproject.org/
- Порівняння YAML vs. JSON vs. XML: https://www.upwork.com/resources/json-vs-xml-vs-yaml
