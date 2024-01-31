---
title:                "Створення текстового файлу"
date:                  2024-01-19
html_title:           "Arduino: Створення текстового файлу"
simple_title:         "Створення текстового файлу"

category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Запис текстового файлу - це процес створення або оновлення файлу даними у текстовому форматі. Програмісти це роблять для зберігання даних, ведення логів, конфігурацій програм та обміну інформацією між системами.

## How to:
Спосіб запису файлу в C#:

```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string filePath = "example.txt";
        string textToWrite = "Привіт, це тестовий файл!";
        
        File.WriteAllText(filePath, textToWrite);
        Console.WriteLine("Файл записано!");
    }
}
```

Коли запустите, вивід буде:
```
Файл записано!
```

Якщо вам потрібно додати текст у файл:

```C#
string textToAdd = "\nДодатковий рядок.";
File.AppendAllText(filePath, textToAdd);
```

## Deep Dive:
У минулому для запису файлів часто використовувались потоки (streams) та письменники даних (StreamWriter), які і досі корисні для великих даних або для потокової обробки. Альтернативами є бібліотеки вищого рівня або мовні конструкції, як System.Xml для XML файлів чи System.Json для JSON. Вибір методу залежить від розміру даних і потреби в контролі над процесом запису.

## See Also:
- Microsoft документація по класу File: [File Class (System.IO)](https://docs.microsoft.com/en-us/dotnet/api/system.io.file)
- Документація по StreamWriter: [StreamWriter (System.IO)](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamwriter)
- Використання файлових потоків (FileStream): [FileStream Class (System.IO)](https://docs.microsoft.com/en-us/dotnet/api/system.io.filestream)
