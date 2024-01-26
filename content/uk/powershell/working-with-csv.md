---
title:                "Робота з CSV файлами"
html_title:           "Arduino: Робота з CSV файлами"
simple_title:         "Робота з CSV файлами"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/working-with-csv.md"
---

{{< edit_this_page >}}

## Що та Чому?
CSV, або "Comma-Separated Values", — це простий формат файлів для таблиць. Програмісти використовують CSV через його універсальність та легкість обробки в різних мовах програмування, включаючи PowerShell.

## Як це зробити:
Створення CSV файлу з даними:
```PowerShell
$data = @(
    [PSCustomObject]@{Name='John'; Age=30; City='Kyiv'},
    [PSCustomObject]@{Name='Olena'; Age=25; City='Lviv'}
)
$data | Export-Csv -Path 'users.csv' -NoTypeInformation
```

Завантаження даних з CSV файлу:
```PowerShell
$users = Import-Csv -Path 'users.csv'
$users
```

Вивід:
```
Name  Age City
----  --- ----
John  30  Kyiv
Olena 25  Lviv
```

Фільтрація і обробка даних з CSV:
```PowerShell
$usersAged25 = Import-Csv -Path 'users.csv' | Where-Object { $_.Age -eq 25 }
$usersAged25
```

## Поглиблений погляд
CSV виник у 1970-х як зручний спосіб обміну структурованими даними. Сьогодні існує багато альтернатив, наприклад, JSON, XML, або бази даних SQL, які можна використовувати для складніших або більш специфічних задач. У PowerShell, робота з CSV здійснюється за допомогою cmdlet -ів `Import-Csv` та `Export-Csv`, які використовуються для читання та запису відповідно. Обробка таких файлів зазвичай використовує об'єктну модель PowerShell, що робить маніпуляцію даними гнучкою.

## Дивись також
- [Import-Csv (Microsoft Docs)](https://docs.microsoft.com/powershell/module/microsoft.powershell.utility/import-csv)
- [Export-Csv (Microsoft Docs)](https://docs.microsoft.com/powershell/module/microsoft.powershell.utility/export-csv)
