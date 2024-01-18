---
title:                "Розбір дати зі стрічки"
html_title:           "PowerShell: Розбір дати зі стрічки"
simple_title:         "Розбір дати зі стрічки"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Що таке та навіщо?

Розбір дати з рядка - це процес отримання дати з рядкового значення. В програмуванні це дуже важливий процес, оскільки часто дати зберігаються у вигляді рядків. Тому розбір дати з рядка дозволяє нам отримати коректну дату для подальшої обробки в нашій програмі.

## Як?

```PowerShell
# Приклад 1: Розбір дати з рядка у форматі "dd/MM/yyyy"
$dateString = "25/06/2020"
$date = [DateTime]::ParseExact($dateString, "dd/MM/yyyy", $null)
Write-Output $date
# Виведе "Thursday, June 25, 2020 12:00:00 AM"

# Приклад 2: Розбір дати з рядка у форматі "ddd, d MMM yyyy"
$dateString = "Thu, 25 Jun 2020"
$date = [DateTime]::ParseExact($dateString, "ddd, d MMM yyyy", $null)
Write-Output $date
# Виведе "Thursday, June 25, 2020 12:00:00 AM"
```

## Глибоке занурення

Розбір дати з рядка є складним процесом, оскільки існує багато різних форматів дати. У минулому для цього використовувалися ручні методи, однак зараз існує багато готових функцій для розбору дати з рядка. Також,  існує багато альтернативних шляхів розбору дати, наприклад, використання регулярних виразів.

Щоб розбирати дату з рядка у PowerShell, ми використовуємо статичний метод [DateTime]::ParseExact(), який приймає три аргументи: рядок з датою, формат дати та об'єкт для обробки помилок.

## Дивись також

[Microsoft довідковий посібник](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/parseexact) про розбір дати з рядка у PowerShell.