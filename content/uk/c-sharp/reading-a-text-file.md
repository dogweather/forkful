---
title:                "C#: Читання текстового файлу"
simple_title:         "Читання текстового файлу"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Чому

Програмістам часто доводиться стикатися з обробкою великих обсягів даних, які зберігаються у текстових файлах. Чи треба читати всю цю інформацію вручну? Ні! З допомогою С#, ви можете легко і швидко зчитувати текстові файли та обробляти їх дані за допомогою програмування.

## Як це зробити

Для початку, вам знадобиться клас `StreamReader` для читання текстового файлу. Нижче наведений приклад коду з використанням цього класу:

```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        // Відкриваємо текстовий файл для читання
        using (StreamReader file = new StreamReader("test.txt"))
        {
            // Файл зчитується рядок за рядком
            string line;
            while ((line = file.ReadLine()) != null)
            {
                // Виводимо рядок на консоль
                Console.WriteLine(line);
            }
        }
    }
}
```

Вищевказаний код відкриє файл з назвою "test.txt" та виведе його вміст на консоль. Зверніть увагу на ключове слово `using`, яке дозволяє автоматично закрити файл після встановлення з'єднання.

## Занурення у глибину

Крім методу `ReadLine()`, клас `StreamReader` має багато інших корисних методів для читання текстових файлів. Наприклад, метод `ReadToEnd()` дозволяє зчитати весь файл зразу, а метод `Read()` дозволяє зчитувати по одному символу за раз. Детальніше про ці методи та їх використання можна дізнатися в [офіційній документації](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamreader?view=net-5.0).

## Дивіться також

- [Офіційна документація Microsoft для класу StreamReader](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamreader?view=net-5.0)
- [Туторіал по роботі з текстовими файлами в C#](https://www.c-sharpcorner.com/article/reading-and-writing-text-file-using-streamreader-and-streamwriter-in-c-sharp/)