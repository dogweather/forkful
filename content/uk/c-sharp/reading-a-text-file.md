---
title:    "C#: Читання текстового файлу"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Чому

В програмуванні, робота з текстовими файлами є невідємною частиною багатьох проектів. Читання текстового файлу дозволяє отримати доступ до вмісту, що зберігається у файлі і обробити його згідно з вимогами проекту. Також, це може бути корисно при обробці великих об'ємів даних, які забезпечуються через текстовий файл.

## Як це зробити

Для читання текстового файлу використовується метод `File.ReadAllText()`. Перш за все, необхідно включити простір імен `System.IO`, щоб отримати доступ до класу `File`. Далі треба вказати шлях до нашого файлу і зберегти його у змінну типу `string`.

```C#
using System.IO;

string path = @"C:\Users\user\Documents\file.txt";
string fileContent = File.ReadAllText(path);

Console.WriteLine(fileContent);
```

В результаті, ми отримаємо вміст нашого файлу і виведемо його на консоль.

## Глибше занурення

Крім методу `File.ReadAllText()`, існує ще кілька способів читання текстового файлу в C#. Наприклад, клас `StreamReader` дозволяє читати файл по рядках, що може бути корисним при обробці великих файлів.

```C#
using System.IO;

string path = @"C:\Users\user\Documents\file.txt";

// Читання файлу по рядках
using (StreamReader reader = new StreamReader(path))
{
    string line;
    while ((line = reader.ReadLine()) != null)
    {
        Console.WriteLine(line);
    }
}

// Читання всього файлу
string fileContent = File.ReadAllText(path);

// Читання великих файлів по частинках
using (StreamReader reader = new StreamReader(path))
{
    char[] buffer = new char[1024];
    while (reader.Read(buffer, 0, buffer.Length) > 0)
    {
        Console.WriteLine(buffer);
    }
}
```

Також, є можливість використовувати різні кодування для читання файлу, що дозволяє правильно відображати українські літери та особливі символи.

## Дивись також

- [Офіційна документація Microsoft для роботи з файлами в C#](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/file-system/how-to-read-from-a-text-file)
- [Стаття про роботу з файлами в C# на сайті Tproger](https://tproger.ru/articles/files-in-c-sharp/)
- [Відеоурок про читання та запис файлів в C# на каналі Tim Corey](https://www.youtube.com/watch?v=aypLot5zPXA)