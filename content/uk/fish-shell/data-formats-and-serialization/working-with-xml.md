---
date: 2024-01-26 04:31:25.634550-07:00
description: "\u042F\u043A: Fish \u043D\u0435 \u043C\u0430\u0454 \u0432\u0431\u0443\
  \u0434\u043E\u0432\u0430\u043D\u043E\u0433\u043E \u043F\u0430\u0440\u0441\u0438\u043D\
  \u0433\u0443 XML, \u0442\u043E\u043C\u0443 \u0432\u0430\u043C \u0434\u043E\u0432\
  \u0435\u0434\u0435\u0442\u044C\u0441\u044F \u0441\u043F\u0438\u0440\u0430\u0442\u0438\
  \u0441\u044F \u043D\u0430 \u0437\u043E\u0432\u043D\u0456\u0448\u043D\u0456 \u0456\
  \u043D\u0441\u0442\u0440\u0443\u043C\u0435\u043D\u0442\u0438 \u043D\u0430 \u043A\
  \u0448\u0442\u0430\u043B\u0442 `xmllint` \u0430\u0431\u043E `xmlstarlet`. \u041E\
  \u0441\u044C \u0444\u0440\u0430\u0433\u043C\u0435\u043D\u0442 \u0434\u043B\u044F\
  \u2026"
lastmod: '2024-03-13T22:44:50.111269-06:00'
model: gpt-4-0125-preview
summary: "Fish \u043D\u0435 \u043C\u0430\u0454 \u0432\u0431\u0443\u0434\u043E\u0432\
  \u0430\u043D\u043E\u0433\u043E \u043F\u0430\u0440\u0441\u0438\u043D\u0433\u0443\
  \ XML, \u0442\u043E\u043C\u0443 \u0432\u0430\u043C \u0434\u043E\u0432\u0435\u0434\
  \u0435\u0442\u044C\u0441\u044F \u0441\u043F\u0438\u0440\u0430\u0442\u0438\u0441\u044F\
  \ \u043D\u0430 \u0437\u043E\u0432\u043D\u0456\u0448\u043D\u0456 \u0456\u043D\u0441\
  \u0442\u0440\u0443\u043C\u0435\u043D\u0442\u0438 \u043D\u0430 \u043A\u0448\u0442\
  \u0430\u043B\u0442 `xmllint` \u0430\u0431\u043E `xmlstarlet`."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 XML"
weight: 40
---

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
