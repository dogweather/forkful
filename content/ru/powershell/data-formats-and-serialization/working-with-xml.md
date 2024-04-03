---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:18.829737-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: ."
lastmod: '2024-03-13T22:44:45.496411-06:00'
model: gpt-4-0125-preview
summary: .
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 XML"
weight: 40
---

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
