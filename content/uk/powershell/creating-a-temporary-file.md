---
title:                "Створення тимчасового файлу"
html_title:           "PowerShell: Створення тимчасового файлу"
simple_title:         "Створення тимчасового файлу"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

Що & За що?
Створення тимчасового файлу - це процес створення короткострокового файлу, який використовується для зберігання даних на час виконання програми. Програмісти часто створюють тимчасові файли для тимчасового зберігання або обробки даних без необхідності створювати постійний файл.

Як це зробити?
Використовуючи PowerShell, створення тимчасового файлу є простим та швидким процесом. Для цього вам потрібно використовувати команду ```New-TemporaryFile``` та вказати шлях для збереження файлу. Наприклад:

```PowerShell
$tempFile = New-TemporaryFile -Path C:\Users\User\Desktop
```
Це створить тимчасовий файл на робочому столі з випадково згенерованим іменем. Ви можете отримати повний шлях до створеного файлу, використовуючи властивість ```FullName```:

```PowerShell
$tempFile.FullName
```

Глибші деталі
Концепція створення тимчасових файлів існує вже давно і використовується в багатьох мовах програмування. Крім використання команди ```New-TemporaryFile```, ви також можете створити тимчасовий файл, використовуючи функцію ```System.IO.Path.GetTempFile()``` в мові C#.

Як альтернатива, ви можете створити постійний файл та встановити атрибут "тимчасовий" для нього, щоб вказати, що цей файл має бути видалений після використання.

Необхідно враховувати, що тимчасові файли можуть бути видалені автоматично після завершення сеансу PowerShell або в разі непередбачених помилок.

Див. також
- [New-TemporaryFile documentation](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/new-temporaryfile?view=powershell-7)
- [System.IO.Path.GetTempFile() documentation](https://docs.microsoft.com/en-us/dotnet/api/system.io.path.gettempfilename?view=netframework-4.8)