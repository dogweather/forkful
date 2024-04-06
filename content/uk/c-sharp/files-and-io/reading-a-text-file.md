---
date: 2024-01-20 17:54:23.671014-07:00
description: "\u042F\u043A \u0446\u0435 \u0440\u043E\u0431\u0438\u0442\u0438: **\u0412\
  \u0438\u0445\u0456\u0434\u043D\u0456 \u0434\u0430\u043D\u0456:**."
lastmod: '2024-04-05T21:53:49.500826-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u0427\u0438\u0442\u0430\u043D\u043D\u044F \u0442\u0435\u043A\u0441\u0442\u043E\
  \u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0443"
weight: 22
---

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
