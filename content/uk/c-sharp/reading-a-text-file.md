---
title:                "C#: Читання текстового файлу"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

##Чому
Читання текстового файлу є важливою навичкою в програмуванні, оскільки дозволяє отримувати та обробляти великі обсяги даних. Це особливо корисно для роботи зі структурованими даними, такими як CSV або JSON файли.

##Як
Для читання текстового файлу використовується клас `StreamReader`, який забезпечує зручний інтерфейс для читання послідовності символів з файлу. Приклад коду для читання файлу та виведення його вмісту на консоль можна побачити нижче:

```C#
using System;
using System.IO;

namespace FileReading
{
    class Program
    {
        static void Main(string[] args)
        {
            // Відкриваємо файл для читання
            using (StreamReader sr = new StreamReader("file.txt"))
            {
                // Читаємо файл по рядках доки не досягнемо кінця файлу
                while (!sr.EndOfStream)
                {
                    // Зчитуємо рядок з файлу
                    string line = sr.ReadLine();
                    // Виводимо рядок на консоль
                    Console.WriteLine(line);
                }
            }
        }
    }
}
```

Після запуску програми на консоль буде виведено вміст файлу `file.txt`.

##Глибше
Крім читання рядків з текстового файлу, `StreamReader` має багато інших корисних методів, наприклад, `Read()`, `ReadBlock()`, `Peek()`, які дозволяють читати символи або блоки символів з файлу. Крім того, можна вказати кодування файлу при створенні `StreamReader`, щоб правильно читати текст з файлу.

##Дивіться також
- [Документація C# для класу StreamReader](https://docs.microsoft.com/uk-ua/dotnet/api/system.io.streamreader)
- [Робота з текстовими файлами в C#](https://metanit.com/sharp/tutorial/11.1.php)
- [Порівняння методів читання файлів в C#](https://devblogs.microsoft.com/premier-developer/reading-files-in-csharp/)