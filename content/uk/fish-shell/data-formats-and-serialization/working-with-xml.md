---
title:                "Робота з XML"
aliases:
- /uk/fish-shell/working-with-xml/
date:                  2024-01-26T04:31:25.634550-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/working-with-xml.md"
---

{{< edit_this_page >}}

## Що і чому?
Робота з XML означає маніпуляції з даними в усюдисущому, структурованому форматі, який використовують у конфігураціях, меседжингу та багатьому іншому. Програмісти маніпулюють XML для читання, запису, оновлення та запиту даних — життєво важливо для взаємодії безлічі додатків та сервісів.

## Як:
Fish не має вбудованого парсингу XML, тому вам доведеться спиратися на зовнішні інструменти на кшталт `xmllint` або `xmlstarlet`. Ось фрагмент для читання значень:

```fish
# Parse XML using xmlstarlet
echo '<root><element>Hello World</element></root>' | xmlstarlet sel -t -v "/root/element"
```

Вивід:
```
Hello World
```

Для редагування XML скористайтеся цим:

```fish
# Edit XML element using xmlstarlet
echo '<root><element>Old Value</element></root>' | xmlstarlet ed -u "/root/element" -v 'New Value'
```

Вивід:
```xml
<?xml version="1.0"?>
<root>
  <element>New Value</element>
</root>
```

## Поглиблений аналіз:
XML існує з кінця 90-х років, створений для читабельності та дружелюбності до машин. Хоча JSON витіснив частину популярності XML завдяки своїй простоті, XML залишається вкоріненим там, де ключовими є валідація документів та простори імен.

Альтернативи? Звісно — JSON, YAML, або навіть бінарні формати на кшталт Protocol Buffers для тих додатків, де важлива продуктивність. Але схема XML та XSLT (для трансформацій XML) можуть бути вирішальними для складних сценаріїв, де має значення надійність.

Під капотом, інструменти на кшталт `xmlstarlet` користуються потужними бібліотеками на кшталт libxml2, надаючи вам XPath та XQuery для детальної роботи з XML. Це не просто інструменти для XML, а шлюзи до маніпулювання DOM, адже ви застосовуватимете схожі концепти у будь-якій мові, яка торкається XML.

## Дивіться також:
- [Документація xmlstarlet](http://xmlstar.sourceforge.net/doc/UG/xmlstarlet-ug.html)
- [Документація Fish](https://fishshell.com/docs/current/index.html)
- [Функції та оператори XPath та XQuery](https://www.w3.org/TR/xpath-functions/)
