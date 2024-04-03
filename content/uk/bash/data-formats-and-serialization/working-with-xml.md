---
date: 2024-01-26 04:28:12.767523-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u041E\u0441\u044C \u044F\u043A \u0430\u043D\u0430\u043B\u0456\u0437\u0443\u0432\
  \u0430\u0442\u0438 XML \u0443 Bash. \u0406\u043D\u0441\u0442\u0440\u0443\u043C\u0435\
  \u043D\u0442\u0438? xmllint \u0442\u0430 xmlstarlet. \u041F\u0440\u043E\u0445\u043E\
  \u0434\u0436\u0435\u043D\u043D\u044F \u0447\u0435\u0440\u0435\u0437 \u0435\u043B\
  \u0435\u043C\u0435\u043D\u0442\u0438 XML? \u041E\u0434\u043D\u043E\u0437\u043D\u0430\
  \u0447\u043D\u043E. \u041F\u0440\u0438\u043A\u043B\u0430\u0434 \u0456\u0437 \u043F\
  \u0440\u0438\u043A\u043B\u0430\u0434\u043D\u0438\u043C\u2026"
lastmod: '2024-03-13T22:44:49.619056-06:00'
model: gpt-4-0125-preview
summary: "\u041E\u0441\u044C \u044F\u043A \u0430\u043D\u0430\u043B\u0456\u0437\u0443\
  \u0432\u0430\u0442\u0438 XML \u0443 Bash."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 XML"
weight: 40
---

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
