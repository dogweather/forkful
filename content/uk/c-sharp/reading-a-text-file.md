---
title:                "Читання текстового файлу"
html_title:           "Arduino: Читання текстового файлу"
simple_title:         "Читання текстового файлу"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Що і чому?

Читання текстового файлу є процесом вилучення даних із текстового файлу, програмно реалізованим у коді. Програмісти це роблять, щоб маніпулювати даними, збереженими у форматі тексту, або щоб завантажити конфігураційні параметри для свого програмного забезпечення.

## Як це зробити:

Приклад читання текстового файлу в C#:

```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string path = @"C:\YourDir\YourFile.txt";

        string readText = File.ReadAllText(path);
        Console.WriteLine(readText);
    }
}
```
Вищевказаний код відкриває файл `YourFile.txt`, читає його повністю, а потім видає його вміст у консоль. 

## Поглиблений матеріал

* Історичний контекст: Функція читання текстового файлу завжди була важливою частиною програмування, так як текстові файли є одним з найперших методів збереження і передачі даних.
* Альтернативи: Існують інші методики читання тектового файлу, наприклад, використання `StreamReader`, який може бути більш ефективним для великих файлів. Також `File.ReadLines()` може бути використано для ітерації по рядках.
* Подробиці реалізації: `File.ReadAllText()` читає текстовий файл повністю в пам'ять перед тим, як його повернути. Це спрощує читання файлів, але може створити проблеми з використанням пам'яті для дуже великих файлів.

## Додатково

Слідкуйте за цими посиланнями, щоб отримати додаткову інформацію:

* [C# StreamReader Class](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamreader?view=net-5.0)
* [File.ReadLines Method in C#](https://docs.microsoft.com/en-us/dotnet/api/system.io.file.readlines?view=net-5.0)