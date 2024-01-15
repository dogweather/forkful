---
title:                "Створення тимчасового файлу"
html_title:           "C#: Створення тимчасового файлу"
simple_title:         "Створення тимчасового файлу"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## З чого почати

Створення тимчасового файлу - це корисний процес, який може бути використаний в багатьох програмах. Чи є це тестування програмного забезпечення або зберігання тимчасової інформації, створення тимчасового файлу допоможе вам здійснити вашу задачу з легкістю.

## Як це зробити

```C#
using System;
using System.IO;

namespace TemporaryFile
{
    class Program
    {
        static void Main(string[] args)
        {
            // Створення тимчасового файлу у директорії за замовчуванням
            string tempFile = Path.GetTempFileName();
            Console.WriteLine("Тимчасовий файл створено: {0}", tempFile);

            // Записати у тимчасовий файл
            using (StreamWriter writer = new StreamWriter(tempFile))
            {
                writer.WriteLine("Вітаю, це тимчасовий файл!");
            }

            // Прочитати дані з тимчасового файлу
            using (StreamReader reader = new StreamReader(tempFile))
            {
                string contents = reader.ReadToEnd();
                Console.WriteLine("Вміст тимчасового файлу: {0}", contents);
            }

            // Видалити тимчасовий файл
            File.Delete(tempFile);
            Console.WriteLine("Тимчасовий файл успішно видалено!");
        }
    }
}
```

Вищенаведений код показує, як створити, записати та прочитати дані з тимчасового файлу за допомогою класів з простору імен System.IO. Зверніть увагу, що після використання тимчасового файлу, його необхідно видалити за допомогою методу File.Delete().

## Глибше

Створення тимчасового файлу слід використовувати з обережністю, оскільки це може призвести до зайвого використання місця на диску. Краще використовувати цей процес, коли необхідно працювати з невеликими об'ємами даних або як тимчасовий засіб для розв'язання певної задачі. Крім того, навіть видаляючи тимчасовий файл після використання, необхідно впевнитися, що він був видалений успішно, щоб уникнути ситуації, коли він залишається на диску і споживає місце.

## Дивіться також

- [Документація Microsoft про створення тимчасових файлів в C#](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/file-system/how-to-create-a-temporary-file)
- [Більш докладний опис тимчасових файлів на Wikipedia](https://en.wikipedia.org/wiki/Temporary_file)