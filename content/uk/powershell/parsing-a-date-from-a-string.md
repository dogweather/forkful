---
title:                "Аналіз дати з рядка"
html_title:           "C++: Аналіз дати з рядка"
simple_title:         "Аналіз дати з рядка"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Що та навіщо?
Використовуючи PowerShell, нерідко виникає необхідність аналізувати дату з рядка. Цей процес називається "parsing". Програмісти роблять це для сортування, порівняння або просто керування датами в рядках.

## Як це робити:
Ось простий приклад коду, який показує, як виконується процес:
```PowerShell
$StringDate = '01-02-2021'
$ParsedDate = [datetime]::ParseExact($StringDate, 'dd-MM-yyyy', $null)
Write-Output $ParsedDate
```
При виконанні цього скрипта, ви отримаєте наступний результат:
```PowerShell
Sunday, February 1, 2021 12:00:00 AM
```

## Глибоке занурення:
Перед введенням PowerShell, программісти використовували різні способи аналізу дати з рядка, наприклад прийоми з JScript або VBScript. Але PowerShell вносить у цей процес значні полегшення.

Наприклад, альтернативою для ```[datetime]::ParseExact``` є ```[datetime]::TryParseExact```, що повертає булеве значення, яке вказує, чи було успішним аналізування рядка або ні.

Якщо рядок містить фрагменти, які потрібно проігнорувати при аналізуванні, то випарсити дату можна за допомогою його формату:
```PowerShell
$StringDate = 'Дата: 01-02-2021'
$ParsedDate = [datetime]::ParseExact($StringDate, "'Дата: 'dd-MM-yyyy", $null)
Write-Output $ParsedDate
```

## Додатково:
Чудове джерело для додаткового вивчення цієї теми - офіційна документація Microsoft, особливо сторінка ParseExact: https://docs.microsoft.com/en-us/dotnet/api/system.datetime.parseexact?view=netframework-4.7.2