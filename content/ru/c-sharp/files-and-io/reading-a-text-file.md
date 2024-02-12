---
title:                "Чтение текстового файла"
aliases: - /ru/c-sharp/reading-a-text-file.md
date:                  2024-01-29T00:00:54.227763-07:00
model:                 gpt-4-0125-preview
simple_title:         "Чтение текстового файла"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/c-sharp/reading-a-text-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Зачем?
Чтение текстового файла — это извлечение данных из файла, содержащего текст. Программисты делают это для загрузки конфигураций, чтения данных или получения ресурсов, которые слишком громоздки или неуместно закодировать напрямую.

## Как это сделать:
Давайте перейдем непосредственно к делу. Вот как можно прочитать файл в C# с использованием `System.IO`.

```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string filePath = @"C:\path\to\your\file.txt";
        
        // Чтение всего текста
        string allText = File.ReadAllText(filePath);
        Console.WriteLine(allText);
        
        // Чтение строк в массив
        string[] lines = File.ReadAllLines(filePath);
        foreach (var line in lines)
        {
            Console.WriteLine(line);
        }
        
        // Чтение с помощью StreamReader
        using (StreamReader reader = new StreamReader(filePath))
        {
            string line;
            while ((line = reader.ReadLine()) != null)
            {
                Console.WriteLine(line);
            }
        }
    }
}
```

Пример вывода:

```
Привет, это текстовый файл.
Здесь много строк.
Каждая строка будет прочитана отдельно.
```

## Подробнее
Кажется, чтение текстового файла достаточно просто, правда? Но есть немного истории и некоторые нюансы, которые стоит знать.

В прошлые времена текстовые файлы часто были основным способом хранения данных до широкого использования баз данных. Программистам приходилось управлять доступом к файлам, корректно форматировать данные и обрабатывать ошибки. C# многое изменилось с тех пор. Теперь `System.IO` - это ваше главное пространство имен для операций с файлами.

У вас есть варианты:

- `File.ReadAllText` читает весь файл за один раз — отлично подходит для меньших файлов.
- `File.ReadAllLines` предоставляет каждую строку как элемент массива — удобно для обработки строк.
- `StreamReader` читает построчно, что более эффективно использует память для больших файлов.

Каждый метод блокирует файл во время его использования. Это важно, если другие процессы могут пытаться получить доступ к файлу.

Помните, всегда обрабатывайте исключения, такие как `FileNotFoundException` или `IOException`, при работе с файлами. Вы не хотите, чтобы ваше приложение неожиданно завершилось с ошибкой.

## Смотрите также
Есть больше вопросов или хотите расширить свои знания? Проверьте эти ссылки:

- [Документация MSDN о классе File](https://docs.microsoft.com/en-us/dotnet/api/system.io.file?view=netcore-3.1)
- [Документация MSDN о классе StreamReader](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamreader?view=netcore-3.1)
- [Учебник по обработке исключений](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/exceptions/)
