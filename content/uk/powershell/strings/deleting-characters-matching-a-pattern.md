---
date: 2024-01-20 17:43:06.886460-07:00
description: "\u042F\u043A \u0446\u0435 \u0440\u043E\u0431\u0438\u0442\u0438: \u0412\
  \u0438\u0432\u0435\u0434\u0435: `Hello, ths s an exmple!`."
lastmod: '2024-04-05T21:53:49.753934-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u0412\u0438\u0434\u0430\u043B\u0435\u043D\u043D\u044F \u0441\u0438\u043C\u0432\
  \u043E\u043B\u0456\u0432 \u0437\u0430 \u0432\u0456\u0437\u0435\u0440\u0443\u043D\
  \u043A\u043E\u043C"
weight: 5
---

## Як це робити:
```PowerShell
# Використовуємо -replace для видалення небажаних символів
$text = "Hello, th1s 1s an ex4mple!"
$pattern = '[0-9]'
$text = $text -replace $pattern, ''
Write-Output $text
```
Виведе: `Hello, ths s an exmple!`

```PowerShell
# Видаляємо спеціальні символи
$text = "Data! Lots of data# Everywhere?"
$pattern = '[:punct:]'
$text = $text -replace $pattern, ''
Write-Output $text
```
Виведе: `Data Lots of data Everywhere`

## Підводне каміння:
Шаблони, які ми використовуємо для видалення символів, базуються на регулярних виразах - потужному інструменті, що виник ще в 1950-х як частина теоретичних досліджень в області комп'ютерних наук. PowerShell, як і багато інших мов програмування, включає в себе бібліотеку для регулярних виразів, що дозволяє швидко і гнучко обробляти текст. Альтернативами "-replace" можуть бути методи .trim(), .substring() та інші методи .NET для стрічок, але вони не завжди такі потужні або гнучкі, як регулярні вирази. Деталі регулярних виразів та їхній синтаксис можуть бути складними, але вивчення їх розширює можливості маніпуляції з текстом.

## Дивіться також:
* [Regular-Expressions.info's overview of regex in PowerShell](https://www.regular-expressions.info/powershell.html)
