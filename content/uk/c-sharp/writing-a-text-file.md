---
title:                "Створення текстового файлу"
html_title:           "C#: Створення текстового файлу"
simple_title:         "Створення текстового файлу"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Чому

Написання текстового файлу є невід'ємною частиною багатьох програм, де необхідно зберігати дані в розширенні, зручному для зберігання і обробки. Наприклад, збереження налаштувань, щоб користувач міг повернутися до них у майбутньому або створення та зчитування лог-файлів для відстеження дії програми.

## Як

```C#
using System;
using System.IO;

// створення і запис до текстового файлу
using (StreamWriter file = new StreamWriter("file.txt"))
{
    file.WriteLine("Привіт, світ!");
}

// зчитування з текстового файлу
string[] lines = File.ReadAllLines("file.txt");
foreach (string line in lines)
{
    Console.WriteLine(line);
}

// видалення текстового файлу
File.Delete("file.txt");

// використання StreamWriter для додавання нової інформації до файлу
using (StreamWriter file = new StreamWriter("file.txt", true))
{
    file.WriteLine("Цей рядок буде доданий до файлу.");
}

// повторне зчитування файлу, щоб перевірити зміни
string[] newLines = File.ReadAllLines("file.txt");
foreach (string line in newLines)
{
    Console.WriteLine(line);
}
```

В результаті виконання цих прикладів коду, у вас буде створений файл з назвою "file.txt" із рядком "Привіт, світ!" у першому прикладі та доданим рядком "Цей рядок буде доданий до файлу." у другому прикладі. За допомогою StreamReader та StreamWriter класів з бібліотеки System.IO, ви можете легко створювати, зчитувати та змінювати текстові файли в програмі на мові C#.

## Vertep

Перевірте документацію на [StreamReader](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamreader?view=net-5.0) та [StreamWriter](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamwriter?view=net-5.0) для більш детальної інформації про можливості цих класів. Можна також використовувати методи [File](https://docs.microsoft.com/en-us/dotnet/api/system.io.file?view=net-5.0) класу для роботи з текстовими файлами, такими як створення, зчитування та видалення.