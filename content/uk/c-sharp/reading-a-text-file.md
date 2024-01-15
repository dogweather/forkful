---
title:                "Читання текстового файлу"
html_title:           "C#: Читання текстового файлу"
simple_title:         "Читання текстового файлу"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Чому
Можливо, ви хочете прочитати текстовий файл для обробки даних, які він містить. Наприклад, ви можете використовувати це для створення звіту або аналізу даних.

## Як
Для читання текстового файлу у C#, використовуйте клас `StreamReader` і метод `ReadLine()`. Ось приклад коду, який дозволить вам відкрити та прочитати текстовий файл зі свого комп'ютера:

```C#
using System;
using System.IO;

class Program
{
  static void Main()
  {
    // Відкриття файлу для читання
    using (StreamReader sr = new StreamReader("file.txt"))
    {
      // Зчитування тексту з файлу рядок за рядком
      string line;
      while ((line = sr.ReadLine()) != null)
      {
        // Виконання дій з кожним рядком
        Console.WriteLine(line);
      }
    }
  }
}
```

Приклад вихідного файлу `file.txt`:

```
Це перший рядок.
Це другий рядок.
І це останній рядок.
```

Вихід програми буде виглядати так:

```
Це перший рядок.
Це другий рядок.
І це останній рядок.
```

## Глибинне вивчення
`StreamReader` є частиною простору імен `System.IO` і надає більш високорівневе інтерфейс для зчитування даних з текстового файлу. Його використання є більш зручним, ніж отримання `byte` з файлу та його декодування в `string`. Однак, можливості `StreamReader` не обмежуються лише зчитуванням рядків, ви можете також використовувати його для отримання символів з текстового файлу.

## Дивись також
- [Документація Microsoft про клас StreamReader](https://docs.microsoft.com/uk-ua/dotnet/api/system.io.streamreader)
- [Стаття про роботу з файлами в C#](https://itproger.com/course/csharp-file)