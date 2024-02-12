---
title:                "Робота з XML"
aliases: - /uk/bash/working-with-xml.md
date:                  2024-01-26T04:28:12.767523-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/bash/working-with-xml.md"
---

{{< edit_this_page >}}

## Що і чому?
Робота з XML передбачає аналіз, витягування та маніпулювання даними у форматі Extensible Markup Language. Програмісти борються з XML, оскільки він є поширеним форматом обміну даними для конфігурацій, API та багато іншого.

## Як це зробити:
Ось як аналізувати XML у Bash. Інструменти? xmllint та xmlstarlet. Проходження через елементи XML? Однозначно. Приклад із прикладним виводом:

```bash
# Припустимо, що xmlstarlet встановлено
# Встановіть за допомогою: apt-get install xmlstarlet

# Розбір вмісту XML
cat <<EOF > sample.xml
<fruits>
  <fruit name="Apple"/>
  <fruit name="Banana"/>
</fruits>
EOF

# Витягнення імен за допомогою xmlstarlet
xmlstarlet sel -t -m "//fruit" -v "@name" -n sample.xml

# Вивід має бути:
# Apple
# Banana
```

## Поглиблений аналіз
В кінці 90-х з'явився XML як спрощена альтернатива SGML, але більш структурована, ніж HTML. Тепер у нього є компанія – наприклад, JSON, YAML. Але XML все ще тримається, особливо в конфігураціях та веб-сервісах на основі SOAP.

З точки зору інструментів, xmllint зручний для валідації XML, xpath запитів. xmlstarlet - це швейцарський армійський ніж для XML-фокусів – запит, редагування, валідація, перетворення. У bash-скриптах вони є супергероями для завдань з XML.

Під капотом, xmllint використовує libxml2 – C-парсер XML. Він швидкий, але повідомлення про помилки? Складні для розуміння. А xmlstarlet? Рекурсивні шаблони та підтримка EXSLT. Захоплює розум, але потужно.

## Дивіться також
- [xmlsoft.org](http://xmlsoft.org/): Матеріали про Libxml2 та xmllint.
- [Stack Overflow](https://stackoverflow.com/questions/tagged/xml+bash): Реальні проблеми та рішення.
- [W3Schools XML Tutorial](https://www.w3schools.com/xml/): Основи XML.
