---
title:                "Робота з XML"
date:                  2024-01-26T04:35:05.305822-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з XML"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/working-with-xml.md"
---

{{< edit_this_page >}}

## Що і чому?
Робота з XML передбачає маніпулювання та доступ до даних, структурованих у мові розмітки eXtensible Markup Language. Програмісти працюють з XML для забезпечення взаємодії з іншими системами або для читання та запису файлів конфігурації, потоків даних та інших структурованих документів, які часто зустрічаються у веб-сервісах.

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
