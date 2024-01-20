---
title:                "Перевірка наявності директорії"
html_title:           "C#: Перевірка наявності директорії"
simple_title:         "Перевірка наявності директорії"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Що і Навіщо?
Перевірка на наявність каталогу - це процес, який дозволяє вам з'ясувати, чи існує конкретний каталог на вашому диску. Програмісти роблять це, щоб уникнути помилок вводу-виводу при роботі з файлами.

## Як це зробити:
```C#
using System.IO;

string yourPath = @"C:\your_directory_path";

if(Directory.Exists(yourPath))
{
  Console.WriteLine("Каталог існує.");
}
else
{
  Console.WriteLine("Каталог не існує.");
}
```

Коли виконаєте цей код і ваш каталог існує, ви побачите:
```C#
"Каталог існує."
```
Інакше, ви побачите:
```C#
"Каталог не існує."
```

## Занурення у деталі
Метод `Directory.Exists` був представлений у .NET Framework 1.1 і є основним способом перевірити, чи існує каталог. Існують альтернативні підходи, але вони менш розповсюджені: наприклад, використання `DirectoryInfo.Exists` або обробка виключень при спробі відкрити каталог.

## Дивіться також
Для більш глибокого вивчення теми перегляньте документацію: 
1. Про `Directory.Exists`: [Тут](https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.exists?view=net-5.0).
2. Про `DirectoryInfo.Exists`: [Тут](https://docs.microsoft.com/en-us/dotnet/api/system.io.directoryinfo.exists?view=net-5.0).
3. Про обробку виключень в C#: [Тут](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/exceptions/).