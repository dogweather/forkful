---
title:                "Перетворення дати в рядок"
html_title:           "Lua: Перетворення дати в рядок"
simple_title:         "Перетворення дати в рядок"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Що та навіщо?

Перетворення дати в рядок (стрінг) - це процес встановлення формату дати в текстовий формат. Програмісти роблять це для більшої зручності при відображенні дати, відправці її в інший формат, або для запису в текстовий файл.

## Як це робити:

```PowerShell
# отримуємо поточну дату
$date = Get-Date

# конвертуємо дату в рядок
$dateString = $date.ToString()

# виводимо результат
Write-Output $dateString
```
В результаті ви отримаєте рядок, що відображає поточну дату і час у форматі за замовчуванням, наприклад, "09/16/2021 9:13:50 PM".

```PowerShell
# можемо вказати формат дати при конвертації
$dateString = $date.ToString("dd.MM.yyyy")

# виводимо результат
Write-Output $dateString
```
Зазначений формат перетворить дату в рядок "16.09.2021".

## Занурення в деталі:

Перетворення дати в рядок у PowerShell є частиною .NET Framework, що була введена у 2002 році. Існують інші мови та фреймворки, які також надають можливість виконувати цю дію. Ви також можете використовувати специфікатори форматування дати й часу для досягнення різних форматів.

```PowerShell
# використання специфікатора форматування
$dateString = $date.ToString("MM-dd-yyyy HH:mm:ss")
Write-Output $dateString
```
Цей код перетворить дату в рядок "09-16-2021 21:13:50".

## Дивіться також:

1. [Документація по датах і часу в .Net](https://docs.microsoft.com/uk-ua/dotnet/standard/datetime/)
3. [Функція ToString() в .Net](https://docs.microsoft.com/uk-ua/dotnet/api/system.datetime.tostring)