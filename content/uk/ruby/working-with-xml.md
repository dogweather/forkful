---
title:                "Робота з XML"
aliases:
- uk/ruby/working-with-xml.md
date:                  2024-01-26T04:35:32.058939-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/working-with-xml.md"
---

{{< edit_this_page >}}

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
