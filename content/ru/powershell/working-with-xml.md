---
title:                "Работа с XML"
date:                  2024-01-29T00:05:18.829737-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с XML"

category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/powershell/working-with-xml.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Работа с XML включает в себя манипуляцию и доступ к данным, структурированным на языке eXtensible Markup Language. Программисты работают с XML для обеспечения взаимодействия с другими системами или для чтения и записи файлов конфигураций, потоков данных и других структурированных документов, которые распространены в веб-сервисах.

## Как это сделать:
```PowerShell
# Загрузка XML файла в переменную
[xml]$xmlContent = Get-Content 'path\to\your\file.xml'

# Доступ к узлам XML
$books = $xmlContent.catalog.book
foreach ($book in $books) {
  Write-Output "Название: $($book.title)"
}

# Создание нового элемента XML
$newBook = $xmlContent.CreateElement("book")
$newBook.SetAttribute("id", "bk999")
$xmlContent.DocumentElement.AppendChild($newBook)

# Сохранение XML обратно в файл
$xmlContent.Save('path\to\your\updated\file.xml')
```
Пример вывода:
```
Название: Programming PowerShell
Название: XML Essentials
```

## Глубокое погружение
XML, или eXtensible Markup Language, появился в конце 90-х и остаётся широко используемым форматом для структурированных данных. PowerShell упрощает работу с XML по сравнению с традиционными методами разбора; он преобразует XML непосредственно в объекты, позволяя взаимодействовать с элементами через знакомую точечную нотацию.

Альтернативы XML включают JSON, YAML или пользовательские форматы данных. Например, JSON приобрел популярность за свою легковесность и удобство использования с веб-технологиями. Однако, расширенные возможности XML, такие как пространства имён, схемы и обработка XSLT, часто делают его более подходящим для сложных документов или стандартов отрасли.

PowerShell использует возможности XML .NET Framework для обработки XML. Это означает, что речь идёт не только о простых операциях чтения и записи; вы также можете работать со схемами XML для валидации, использовать XPath для запросов и применять преобразования XSLT, всё через PowerShell.

## Смотрите также
- [Учебник по XML от W3Schools](https://www.w3schools.com/xml/)
- [XML против JSON](https://www.json.org/json-en.html)
