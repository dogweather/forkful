---
title:                "Читання текстового файлу"
aliases: - /uk/c-sharp/reading-a-text-file.md
date:                  2024-01-20T17:54:23.671014-07:00
model:                 gpt-4-1106-preview
simple_title:         "Читання текстового файлу"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Що це таке & Навіщо?
Читання текстового файлу — це процес здобування даних з файлу, який містить текст. Програмісти роблять це для отримання інформації, налаштувань, даних для обробки та іншого.

## Як це робити:
```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        // Шлях до файлу.
        string filePath = "example.txt";
        
        // Читаємо всі рядки (самий безпечний спосіб для коротких файлів).
        try
        {
            string[] lines = File.ReadAllLines(filePath);
            foreach (string line in lines)
            {
                Console.WriteLine(line);
            }
        }
        catch (IOException e)
        {
            Console.WriteLine("Не вдалося прочитати файл:");
            Console.WriteLine(e.Message);
        }
    }
}
```

**Вихідні дані:**
```
// Це приклад тексту в файлі example.txt.
Перший рядок тексту.
Другий рядок тексту.
...

// Виведення в консоль буде аналогічним тексту у файлі.
```

## Поглиблено:
Читання файлів — давня практика. Раніше, коли оперативної пам'яті було мало, файлові системи мали іншу структуру. Сьогодні ми маємо багато способів читання текстових файлів у C#: `File.ReadAllLines()`, `File.ReadAllText()`, `StreamReader` тощо. `File.ReadAllLines()` є зручним для маленьких файлів, а `StreamReader` краще підходить для великих файлів, оскільки він читає дані порціями. Слід пам'ятати про обробку винятків, адже робота з файлами може бути непередбачуваною (файл може бути відсутнім, пошкодженим або заблокованим).

## Дивіться також:
- Microsoft Docs zur "StreamReader" Klasse: [https://docs.microsoft.com/dotnet/api/system.io.streamreader](https://docs.microsoft.com/dotnet/api/system.io.streamreader)
- Microsoft Docs zur "File" Klasse Methoden: [https://docs.microsoft.com/dotnet/api/system.io.file](https://docs.microsoft.com/dotnet/api/system.io.file)
- Stack Overflow: Поширені проблеми при читанні файлів: [https://stackoverflow.com/questions/tagged/c%23+file-io](https://stackoverflow.com/questions/tagged/c%23+file-io)
