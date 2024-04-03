---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:57:45.635569-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0412\u043E\u0442 \u043A\u0430\u043A \u043C\u043E\u0436\u043D\u043E\
  \ \u043D\u0430\u0440\u0435\u0437\u0430\u0442\u044C \u0438 \u0440\u0430\u0441\u043A\
  \u043E\u043B\u043E\u0442\u044C \u0441\u0442\u0440\u043E\u043A\u0438 \u0432 PowerShell."
lastmod: '2024-03-13T22:44:45.426361-06:00'
model: gpt-4-0125-preview
summary: "\u0412\u043E\u0442 \u043A\u0430\u043A \u043C\u043E\u0436\u043D\u043E \u043D\
  \u0430\u0440\u0435\u0437\u0430\u0442\u044C \u0438 \u0440\u0430\u0441\u043A\u043E\
  \u043B\u043E\u0442\u044C \u0441\u0442\u0440\u043E\u043A\u0438 \u0432 PowerShell."
title: "\u0418\u0437\u0432\u043B\u0435\u0447\u0435\u043D\u0438\u0435 \u043F\u043E\u0434\
  \u0441\u0442\u0440\u043E\u043A"
weight: 6
---

## Как это сделать:
Вот как можно нарезать и расколоть строки в PowerShell:

```PowerShell
# Дана строка
$text = "Power up your PowerShell skills!"

# Извлечение с использованием метода substring
$startIndex = 10
$length = 9
$substring = $text.Substring($startIndex, $length)
Write-Host $substring  # Вывод: your Powe

# Извлечение с использованием оператора диапазона
$subrange = $text[10..18] -join ''
Write-Host $subrange  # Вывод: your Powe

# Извлечение с начала до определенной позиции
$firstPart = $text.Substring(0, $startIndex)
Write-Host $firstPart  # Вывод: Power up 

# Извлечение после определенного символа
$splitString = $text.Split(" ")[2]
Write-Host $splitString  # Вывод: your
```

## Подробнее
Давным-давно PowerShell обладал только основными методами обработки строк. Теперь все иначе. Метод `.Substring()` существует уже довольно долго и он достаточно прост в использовании — достаточно указать начальный индекс и, при необходимости, длину, и он вырежет то, что вам нужно. Начиная с PowerShell 6, вы также можете использовать оператор диапазона, который может быть проще, особенно когда вы работаете со строками переменной длины.

Также есть оператор `-split` и метод `.Split()`, оба удобных для разделения строк на основе шаблонов или символов. Нужен конкретный кусок? Используйте эти инструменты.

С точки зрения производительности, для маленьких задач разница незначительна. Когда вы работаете с огромными текстовыми файлами или выполняете цикл каждую миллисекунду, вам понадобятся бенчмарки. В остальных случаях важнее читаемость и то, что кажется правильным для вашего сценария.

Помните, индексация строк в PowerShell начинается с нуля, что является общим для многих языков программирования. Остерегайтесь хитрого ошибки на единицу.

## Смотрите также
Для дополнительной информации о манипуляции со строками в PowerShell:

- [About_Split](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_split?view=powershell-7)
- [About Comparison Operators](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_comparison_operators?view=powershell-7), которое охватывает -split
