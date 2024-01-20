---
title:                "Читання текстового файлу"
html_title:           "Arduino: Читання текстового файлу"
simple_title:         "Читання текстового файлу"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Що і чому?

Читання текстових файлів полягає в отриманні даних, присутніх у цих файлах, за допомогою програм. Програмісти це роблять, щоб маніпулювати і аналізувати ці дані, що важливо для вирішення багатьох задач.

## Як це зробити:

Отже, прочитати текстовий файл у PowerShell можна за допомогою вбудованої команди `Get-Content`. Ось приклад її використання:

```PowerShell
$get_content = Get-Content "C:\Users\Example\example.txt"
$get_content
```

Програма поверне весь вміст вашого файлу `example.txt` рядок за рядком.

## Поглиблений занурення:

Читання файлів — одна із перших операцій, яку виконує комп'ютер. У минулому це вимагало більше коду і часу через обмежені можливості мов програмування.

Альтернативою команді `Get-Content` є команда `.NET` — `System.IO.File]::ReadAllText()`. Вона може бути корисною, коли нам потрібно прочитати файл як єдиний рядок тексту:

```PowerShell
[System.IO.File]::ReadAllText("C:\Users\Example\example.txt")
```

`Get-Content` використовує .NET клас `System.IO.File` у кулісах, працюючи з файлами, але максимально спрощує синтаксис для користувачів PowerShell.

## Диви також: 

1. [Офіційна документація PowerShell по `Get-Content`](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/get-content?view=powershell-7.1).
2. [Детальніше про `System.IO.File` в .NET](https://docs.microsoft.com/en-us/dotnet/api/system.io.file?view=net-5.0).
3. [Тематичний блог про PowerShell](https://devblogs.microsoft.com/scripting/).