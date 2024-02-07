---
title:                "Робота з XML"
date:                  2024-01-26T04:35:19.666028-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/working-with-xml.md"
---

{{< edit_this_page >}}

## Що і чому?
"Робота з XML" відноситься до процесу читання, створення та зміни файлів XML (eXtensible Markup Language, Розширювана Мова Розмітки) за допомогою програмування. Програмісти роблять це, тому що XML широко використовується для обміну даними через його платформо-незалежну природу та самоописний формат.

## Як це зробити:
Модуль `xml.etree.ElementTree` Python пропонує інструменти для роботи з XML.

Розібрати XML-документ:
```python
import xml.etree.ElementTree as ET

xml_data = """<?xml version="1.0"?>
<library>
    <book>
        <title>Learning Python</title>
        <author>Mark Lutz</author>
    </book>
    <book>
        <title>Programming Python</title>
        <author>Mark Lutz</author>
    </book>
</library>
"""

root = ET.fromstring(xml_data)
for book in root.findall('book'):
    title = book.find('title').text
    author = book.find('author').text
    print(f'Назва: {title}, Автор: {author}')
```
Приклад виведення:
```
Назва: Learning Python, Автор: Mark Lutz
Назва: Programming Python, Автор: Mark Lutz
```

Створити XML-документ:
```python
library = ET.Element('library')
book = ET.SubElement(library, 'book')
title = ET.SubElement(book, 'title')
title.text = 'Automate the Boring Stuff with Python'
author = ET.SubElement(book, 'author')
author.text = 'Al Sweigart'

tree = ET.ElementTree(library)
tree.write('library.xml')
```

## Поглиблений розгляд:
XML існує з кінця 90-х років, створений як спрощений підмножина SGML для легкого онлайн обміну даними. Незважаючи на зростаючу популярність JSON для веб-даних, XML залишається важливим у багатьох підприємницьких, конфігураційних та веб-сервісах (SOAP, RSS).

Альтернативи до `xml.etree.ElementTree` включають `lxml` та `minidom`. `lxml` швидший і має більш багатий набір функцій, в той час як `minidom` забезпечує більш "DOM-подібний" XML інтерфейс. При виборі варто враховувати простоту використання, продуктивність та специфічні вимоги до функціоналу.

Під капотом, `ElementTree` працює на моделі дерева елементів, де кожний компонент XML-файлу є вузлом у дереві. Це дозволяє використовувати прості вирази шляху і пошуку, роблячи навігацію та маніпуляцію структурою даних XML легшою.

## Дивись також:
- Модуль Python `xml.etree.ElementTree`: https://docs.python.org/3/library/xml.etree.elementtree.html
- `lxml`: https://lxml.de/
- XML-посібник від W3Schools: https://www.w3schools.com/xml/
- Специфікація XML: https://www.w3.org/XML/
