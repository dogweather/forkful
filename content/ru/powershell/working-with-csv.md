---
title:                "Работа с CSV"
date:                  2024-01-29T00:03:55.896473-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с CSV"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/powershell/working-with-csv.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Работа с CSV (значения, разделенные запятыми) включает в себя обработку текстовых данных, разделенных запятыми, на строки и столбцы. Программисты работают с CSV для обмена данными между программами и системами из-за его простоты и широкой поддержки.

## Как:

### Импорт файла CSV
```PowerShell
$data = Import-Csv -Path "path\to\yourfile.csv"
$data
```
**Пример вывода:**
```
Name        Occupation    Location
----        ----------    --------
Джон Доу    Разработчик   Нью-Йорк
Джейн Смит  Аналитик      Сан-Франциско
```

### Экспорт в файл CSV
```PowerShell
$data | Export-Csv -Path "path\to\newfile.csv" -NoTypeInformation
```
**Создает "newfile.csv" с данными из `$data`.**

### Добавление строки в данные CSV
```PowerShell
$newRow = [PSCustomObject]@{
    Name       = 'Эмили Кларк'
    Occupation = 'Дизайнер'
    Location   = 'Остин'
}
$data += $newRow
$data | Export-Csv -Path "path\to\yourfile.csv" -NoTypeInformation
```

### Выбор определенных столбцов
```PowerShell
$data | Select-Object Name, Location
```
**Пример вывода:**
```
Name        Location
----        --------
Джон Доу    Нью-Йорк
Джейн Смит  Сан-Франциско
Эмили Кларк Остин
```

## Углубленно

Исторически файлы CSV имеют корни в раннем вычислительном процессе как простой способ организации табличных данных без необходимости в сложных форматах файлов. Альтернативы, такие как XML и JSON, предлагают более богатые структуры данных, но CSV выделяется для табличных данных из-за его читаемости, низкой нагрузки и простоты редактирования с помощью простых текстовых редакторов. В PowerShell, командлеты `Import-Csv` и `Export-Csv` инкапсулируют детали реализации, обрабатывая ввод-вывод файлов и конвертацию данных в .NET объекты и из них.

## См. также

- [Документация PowerShell по Import-Csv](https://docs.microsoft.com/ru-ru/powershell/module/microsoft.powershell.utility/import-csv)
- [Документация PowerShell по Export-Csv](https://docs.microsoft.com/ru-ru/powershell/module/microsoft.powershell.utility/export-csv)
