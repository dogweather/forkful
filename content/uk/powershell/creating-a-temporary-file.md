---
title:                "Створення тимчасового файлу"
html_title:           "C: Створення тимчасового файлу"
simple_title:         "Створення тимчасового файлу"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Що та чому?

Створення тимчасового файлу - це практичний спосіб тимчасового зберігання даних в процесі виконання програми. Програмісти роблять це для оптимізації роботи програми, збереження проміжних результатів або відлагодження.

## Як зробити:

Створення тимчасового файлу в PowerShell можна виконати за допомогою командлету `New-TemporaryFile`.

```PowerShell
# Створення нового тимчасового файлу
$tempFile = New-TemporaryFile

# Виведення повного шляху до тимчасового файлу
$tempFile.FullName
```

Після виконання цього сценарію, ви отримаєте повний шлях до створеного тимчасового файлу.

## Поглиблена Інформація

Створюючи тимчасовий файл, ми оперуємо ядром системи, що є таким самим за якого б то не було продукту корпорації Майкрософт, що використовує .NET Framework. 

Альтернатива: ви можете створити тимчасовий файл вручну, використовуючи команду Out-File, але це буде менш автоматизовано.

```PowerShell
# Створення нового тимчасового файлу за допомогою Out-File
$tempFilePath = "$env:TEMP\tempFile.txt"
"Some data" | Out-File -FilePath $tempFilePath -Force
```

У виконанні команди `New-TemporaryFile` .Net Core викликає функцію ядра Windows, що створює тимчасовий файл з унікальним іменем в каталозі для тимчасових файлів, при цьому відслідковуючи деякі метадані.

## Дивіться також

Для отримання більш детальної інформації розгляньте наступні ресурси:

1. Документація PowerShell: https://docs.microsoft.com/uk-ua/powershell/
2. Порівнювач .NET і .NET Core: https://docs.microsoft.com/en-us/dotnet/standard/choosing-core-framework-server
3. Використання тимчасових файлів в Windows: https://www.petri.com/using-temp-files-in-powershell