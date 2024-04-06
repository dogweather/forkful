---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:03:55.896473-07:00
description: "\u041A\u0430\u043A: **\u041F\u0440\u0438\u043C\u0435\u0440 \u0432\u044B\
  \u0432\u043E\u0434\u0430:**."
lastmod: '2024-04-05T21:53:45.902390-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 CSV"
weight: 37
---

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
