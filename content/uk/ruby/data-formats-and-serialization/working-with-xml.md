---
date: 2024-01-26 04:35:32.058939-07:00
description: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 XML \u043E\u0437\u043D\u0430\
  \u0447\u0430\u0454 \u043F\u0430\u0440\u0441\u0438\u043D\u0433, \u0433\u0435\u043D\
  \u0435\u0440\u0430\u0446\u0456\u044E \u0442\u0430 \u043C\u0430\u043D\u0456\u043F\
  \u0443\u043B\u044E\u0432\u0430\u043D\u043D\u044F \u0434\u043E\u043A\u0443\u043C\u0435\
  \u043D\u0442\u0430\u043C\u0438 XML (\u0440\u043E\u0437\u0448\u0438\u0440\u044E\u0432\
  \u0430\u043D\u0430 \u043C\u043E\u0432\u0430 \u0440\u043E\u0437\u043C\u0456\u0442\
  \u043A\u0438) \u0437\u0430 \u0434\u043E\u043F\u043E\u043C\u043E\u0433\u043E\u044E\
  \ \u043A\u043E\u0434\u0443. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\
  \u0442\u0438 \u0440\u043E\u0431\u043B\u044F\u0442\u044C \u0446\u0435 \u0434\u043B\
  \u044F\u2026"
lastmod: '2024-03-13T22:44:50.268868-06:00'
model: gpt-4-0125-preview
summary: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 XML \u043E\u0437\u043D\u0430\
  \u0447\u0430\u0454 \u043F\u0430\u0440\u0441\u0438\u043D\u0433, \u0433\u0435\u043D\
  \u0435\u0440\u0430\u0446\u0456\u044E \u0442\u0430 \u043C\u0430\u043D\u0456\u043F\
  \u0443\u043B\u044E\u0432\u0430\u043D\u043D\u044F \u0434\u043E\u043A\u0443\u043C\u0435\
  \u043D\u0442\u0430\u043C\u0438 XML (\u0440\u043E\u0437\u0448\u0438\u0440\u044E\u0432\
  \u0430\u043D\u0430 \u043C\u043E\u0432\u0430 \u0440\u043E\u0437\u043C\u0456\u0442\
  \u043A\u0438) \u0437\u0430 \u0434\u043E\u043F\u043E\u043C\u043E\u0433\u043E\u044E\
  \ \u043A\u043E\u0434\u0443. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\
  \u0442\u0438 \u0440\u043E\u0431\u043B\u044F\u0442\u044C \u0446\u0435 \u0434\u043B\
  \u044F\u2026"
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 XML"
weight: 40
---

## Що та Чому?
Робота з XML означає парсинг, генерацію та маніпулювання документами XML (розширювана мова розмітки) за допомогою коду. Програмісти роблять це для взаємодії з багатьма веб-сервісами, файлами конфігурації та форматами обміну даними, де XML є спільною мовою.

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
