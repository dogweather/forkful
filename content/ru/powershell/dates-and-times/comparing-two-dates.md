---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:55:45.325122-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0415\u0449\u0435 \u0432 \u043A\u0430\u043C\u0435\u043D\u043D\u043E\
  \u043C \u0432\u0435\u043A\u0435 \u043A\u043E\u043C\u043F\u044C\u044E\u0442\u0438\
  \u0440\u043E\u0432\u0430\u043D\u0438\u044F \u2014 \u043D\u0435 \u0441\u043E\u0432\
  \u0441\u0435\u043C, \u043D\u043E, \u0432\u044B \u043F\u043E\u043D\u0438\u043C\u0430\
  \u0435\u0442\u0435, \u0432 \u0440\u0430\u043D\u043D\u0438\u0435 \u0434\u043D\u0438\
  \ \u2014 \u0434\u0430\u0442\u044B \u0431\u044B\u043B\u0438 \u0437\u0430\u043F\u0443\
  \u0442\u0430\u043D\u043D\u044B\u043C\u0438. \u041C\u044B \u043F\u0440\u043E\u0434\
  \u0435\u043B\u0430\u043B\u0438 \u0434\u043E\u043B\u0433\u0438\u0439 \u043F\u0443\
  \u0442\u044C \u0441\u043E\u2026"
lastmod: '2024-04-05T21:53:45.889187-06:00'
model: gpt-4-0125-preview
summary: "\u0415\u0449\u0435 \u0432 \u043A\u0430\u043C\u0435\u043D\u043D\u043E\u043C\
  \ \u0432\u0435\u043A\u0435 \u043A\u043E\u043C\u043F\u044C\u044E\u0442\u0438\u0440\
  \u043E\u0432\u0430\u043D\u0438\u044F \u2014 \u043D\u0435 \u0441\u043E\u0432\u0441\
  \u0435\u043C, \u043D\u043E, \u0432\u044B \u043F\u043E\u043D\u0438\u043C\u0430\u0435\
  \u0442\u0435, \u0432 \u0440\u0430\u043D\u043D\u0438\u0435 \u0434\u043D\u0438 \u2014\
  \ \u0434\u0430\u0442\u044B \u0431\u044B\u043B\u0438 \u0437\u0430\u043F\u0443\u0442\
  \u0430\u043D\u043D\u044B\u043C\u0438."
title: "\u0421\u0440\u0430\u0432\u043D\u0435\u043D\u0438\u0435 \u0434\u0432\u0443\u0445\
  \ \u0434\u0430\u0442"
weight: 27
---

## Как это сделать:
```PowerShell
# Давайте возьмем текущую дату
$today = Get-Date

# А вот произвольная дата
$someOtherDate = Get-Date "2023-03-17"

# Они равны?
$today -eq $someOtherDate

# Сегодняшняя дата больше (позже), чем другая?
$today -gt $someOtherDate

# Как насчет проверки, если она раньше?
$today -lt $someOtherDate

# Давайте посмотрим результаты, хорошо?

False
True
False
```

## Погружение в детали
Еще в каменном веке компьютирования — не совсем, но, вы понимаете, в ранние дни — даты были запутанными. Мы проделали долгий путь со стандартизацией, и PowerShell упрощает это еще больше.

Вот что стоит усвоить:
1. **История**: Компьютеры раньше обрабатывали даты в разных форматах, что приводило к возможной путанице и ошибкам в стиле Y2K. PowerShell полагается на структуру `DateTime` из .NET, избегая такого хаоса.
   
2. **Альтернативы**: Вы также можете использовать `Compare-Object` или использовать методы из объектов `[datetime]` вроде `.AddDays()`, чтобы выполнить вычисления перед сравнением. Не забудьте использовать `Measure-Command`, чтобы проверить влияние на производительность.
   
3. **Детали реализации**: Даты в PowerShell являются объектами со своими свойствами и методами. Сравнение дат выполняется с использованием операторов (`-eq`, `-lt`, `-gt`), и, благодаря перегрузке операторов, PowerShell знает, что вы имеете дело с датами, а не просто со строками или числами.

На уровне сборки сравнение дат транслируется в тики (интервалы в 100 наносекунд с 1/1/0001). Таким образом, вы, по сути, сравниваете большие целые числа, что является эффективным.

## Смотрите также
- [Структура DateTime (Документация Microsoft)](https://docs.microsoft.com/ru-ru/dotnet/api/system.datetime?view=net-6.0)
- [Работа с датами и временем в PowerShell (SS64.com)](https://ss64.com/ps/syntax-dateformats.html)
