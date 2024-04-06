---
date: 2024-01-26 04:35:05.305822-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u041F\u0440\u0438\u043A\u043B\u0430\u0434 \u0432\u0438\u0432\u043E\u0434\u0443\
  ."
lastmod: '2024-04-05T21:53:49.849158-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 XML"
weight: 40
---

## Як це зробити:
```PowerShell
# Завантаження XML-файла до змінної
[xml]$xmlContent = Get-Content 'шлях\до\вашого\файлу.xml'

# Доступ до вузлів XML
$books = $xmlContent.catalog.book
foreach ($book in $books) {
  Write-Output "Назва: $($book.title)"
}

# Створення нового елемента XML
$newBook = $xmlContent.CreateElement("book")
$newBook.SetAttribute("id", "bk999")
$xmlContent.DocumentElement.AppendChild($newBook)

# Збереження XML назад у файл
$xmlContent.Save('шлях\до\вашого\оновленого\файлу.xml')
```
Приклад виводу:
```
Назва: Programming PowerShell
Назва: XML Essentials
```

## Поглиблений огляд
XML, або eXtensible Markup Language, існує з кінця 90-х і залишається широко використовуваним форматом для структурованих даних. PowerShell спрощує роботу з XML порівняно з традиційними методами парсингу; він безпосередньо перетворює XML на об'єкти, дозволяючи вам взаємодіяти з елементами за допомогою знайомої нотації крапки.

Альтернативи XML включають JSON, YAML або користувацькі формати даних. Наприклад, JSON набув популярності завдяки своїй легкості та зручності у використанні з веб-технологіями. Однак розширені можливості XML, такі як простори імен, схеми та обробка XSLT, часто роблять його більш підходящим для складних документів або стандартів галузі.

PowerShell використовує можливості XML .NET Framework для обробки XML. Це означає, що мова йде не тільки про прості операції читання-запису; ви також можете працювати зі схемами XML для валідації, використовувати XPath для запитів і застосовувати XSLT-трансформації, все через PowerShell.

## Дивіться також
- [W3Schools XML tutorial](https://www.w3schools.com/xml/)
- [XML vs. JSON](https://www.json.org/json-en.html)
