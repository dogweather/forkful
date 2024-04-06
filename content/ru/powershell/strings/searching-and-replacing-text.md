---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:02:13.899934-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: PowerShell \u0434\u0435\u043B\u0430\u0435\u0442 \u043F\u043E\u0438\
  \u0441\u043A \u0438 \u0437\u0430\u043C\u0435\u043D\u0443 \u0434\u043E\u0432\u043E\
  \u043B\u044C\u043D\u043E \u043F\u0440\u043E\u0441\u0442\u044B\u043C\u0438. \u041F\
  \u043E\u0441\u043C\u043E\u0442\u0440\u0438\u0442\u0435 `-replace` \u0434\u043B\u044F\
  \ \u0441\u0442\u0440\u043E\u043A \u0438 `Get-Content` \u0441 `Set-Content` \u0434\
  \u043B\u044F \u0444\u0430\u0439\u043B\u043E\u0432."
lastmod: '2024-03-13T22:44:45.419179-06:00'
model: gpt-4-0125-preview
summary: "PowerShell \u0434\u0435\u043B\u0430\u0435\u0442 \u043F\u043E\u0438\u0441\
  \u043A \u0438 \u0437\u0430\u043C\u0435\u043D\u0443 \u0434\u043E\u0432\u043E\u043B\
  \u044C\u043D\u043E \u043F\u0440\u043E\u0441\u0442\u044B\u043C\u0438."
title: "\u041F\u043E\u0438\u0441\u043A \u0438 \u0437\u0430\u043C\u0435\u043D\u0430\
  \ \u0442\u0435\u043A\u0441\u0442\u0430"
weight: 10
---

## Как это сделать:
PowerShell делает поиск и замену довольно простыми. Посмотрите `-replace` для строк и `Get-Content` с `Set-Content` для файлов.

### Замена текста в строке:
```PowerShell
$text = "I love PowerShell"
$updatedText = $text -replace "love", "adore"
$updatedText
```
Пример вывода:
```
I adore PowerShell
```

### Замена текста в файле:
```PowerShell
$file = "example.txt"
$content = Get-Content $file
$content | ForEach-Object { $_ -replace "oldWord", "newWord" } | Set-Content $file
```
Здесь нет вывода, но теперь в `example.txt` каждое "oldWord" заменено на "newWord".

## Углубленно
Со времен зарождения редактирования текста, поиск и замена были его краеугольным камнем. Это можно сравнить с функцией поиска и замены в текстовом процессоре, но суперзаряженном для нужд программирования.

В старые времена, мастера командной строки использовали инструменты вроде `sed` на Unix. PowerShell внес эту функциональность в свой язык сценариев. Почему это круто? Потому что это связано с объектами, а не только с текстом. Это значит, что вы можете настраивать не только код и текстовые файлы, но и структуры данных и многое другое.

Альтернативы? Конечно. У вас есть текстовые редакторы и IDE с собственным поиском и заменой, пакетные скрипты, или даже программные библиотеки, разработанные для манипуляции с текстом.

Детали реализации? PowerShell использует regex. Это означает, что вы можете заменять содержимое, основываясь на шаблонах, а не только на фиксированных словах. Плюс с помощью сценариев PowerShell вы можете автоматизировать эти операции на огромном количестве файлов, сэкономив кучу времени.

## Смотрите также
- Документация по оператору `-replace` в PowerShell: [ссылка](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_comparison_operators)
- Использование `Get-Content` и `Set-Content`: [ссылка](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/get-content)
