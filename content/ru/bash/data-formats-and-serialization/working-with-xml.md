---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:42.170271-07:00
description: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 XML \u0432\u043A\u043B\u044E\
  \u0447\u0430\u0435\u0442 \u0432 \u0441\u0435\u0431\u044F \u0440\u0430\u0437\u0431\
  \u043E\u0440, \u0438\u0437\u0432\u043B\u0435\u0447\u0435\u043D\u0438\u0435 \u0438\
  \ \u043C\u0430\u043D\u0438\u043F\u0443\u043B\u0438\u0440\u043E\u0432\u0430\u043D\
  \u0438\u0435 \u0434\u0430\u043D\u043D\u044B\u043C\u0438 \u0432 \u0444\u043E\u0440\
  \u043C\u0430\u0442\u0435 Extensible Markup Language. \u041F\u0440\u043E\u0433\u0440\
  \u0430\u043C\u043C\u0438\u0441\u0442\u044B \u0441\u0442\u0430\u043B\u043A\u0438\u0432\
  \u0430\u044E\u0442\u0441\u044F \u0441 XML, \u043F\u043E\u0441\u043A\u043E\u043B\u044C\
  \u043A\u0443\u2026"
lastmod: '2024-03-13T22:44:45.413671-06:00'
model: gpt-4-0125-preview
summary: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 XML \u0432\u043A\u043B\u044E\
  \u0447\u0430\u0435\u0442 \u0432 \u0441\u0435\u0431\u044F \u0440\u0430\u0437\u0431\
  \u043E\u0440, \u0438\u0437\u0432\u043B\u0435\u0447\u0435\u043D\u0438\u0435 \u0438\
  \ \u043C\u0430\u043D\u0438\u043F\u0443\u043B\u0438\u0440\u043E\u0432\u0430\u043D\
  \u0438\u0435 \u0434\u0430\u043D\u043D\u044B\u043C\u0438 \u0432 \u0444\u043E\u0440\
  \u043C\u0430\u0442\u0435 Extensible Markup Language."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 XML"
weight: 40
---

## Как это сделать:
Вот как разобрать XML в Bash. Инструменты? xmllint и xmlstarlet. Циклическое прохождение через элементы XML? Безусловно. Пример с примером вывода:

```bash
# Предполагаем, что xmlstarlet установлен
# Установить при помощи: apt-get install xmlstarlet

# Разбор содержимого XML
cat <<EOF > sample.xml
<fruits>
  <fruit name="Apple"/>
  <fruit name="Banana"/>
</fruits>
EOF

# Извлечение имен при помощи xmlstarlet
xmlstarlet sel -t -m "//fruit" -v "@name" -n sample.xml

# Вывод должен быть:
# Apple
# Banana
```

## Глубокое погружение
В 90-х годах XML появился как более простая альтернатива SGML, но более структурированная, чем HTML. Теперь у него есть компания – например, JSON, YAML. Но XML все еще на коне, особенно в конфигурациях и веб-сервисах, основанных на SOAP.

Что касается инструментария, xmllint удобен для валидации XML, запросов xpath. xmlstarlet - это швейцарский нож для шалостей с XML – запросы, редактирование, валидация, трансформация. В bash-скриптах они являются супергероями для задач по XML.

Под капотом у xmllint используется libxml2 – C-парсер XML. Он быстрый, но сообщения об ошибках? Загадочные. А xmlstarlet? Рекурсивные шаблоны и поддержка EXSLT. Головоломка, но мощная.

## Смотрите также
- [xmlsoft.org](http://xmlsoft.org/): Материалы по Libxml2 и xmllint.
- [Stack Overflow](https://stackoverflow.com/questions/tagged/xml+bash): Реальные проблемы и решения.
- [W3Schools Учебник по XML](https://www.w3schools.com/xml/): Основы XML.
