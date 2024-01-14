---
title:                "C#: Створення тимчасового файлу"
simple_title:         "Створення тимчасового файлу"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

#Чому:

Створення тимчасових файлів є важливим аспектом програмування у C#. Це допомагає оптимізувати і зберігати ресурси, а також захищає від можливих помилок або витоку пам'яті.

##Як створити тимчасовий файл в C#

Існує декілька способів створення тимчасового файлу в C#, але простий із них - використання класу "System.IO.Path". У нього є метод "GetTempFileName()", який автоматично створює унікальне ім'я для тимчасового файлу. Після цього файл можна створити, використовуючи це ім'я.

```C#
string tempFile = System.IO.Path.GetTempFileName();
Console.WriteLine("Новий тимчасовий файл: " + tempFile);

// Output: Новий тимчасовий файл: C:\Users\Username\AppData\Local\Temp\tmpAAC2.tmp
```

Крім цього, можна встановити бажане розширення для тимчасового файлу, використовуючи метод "GetTempPath()" та "GetRandomFileName()".

```C#
string tempPath = System.IO.Path.GetTempPath();
string tempFile = System.IO.Path.GetRandomFileName() + ".txt";
string fullTempPath = System.IO.Path.Combine(tempPath, tempFile);
Console.WriteLine("Новий тимчасовий файл: " + fullTempPath);

// Output: Новий тимчасовий файл: C:\Users\Username\AppData\Local\Temp\bb4nc34x.txt
```

##Глибока погруження

Після створення тимчасового файлу, важливо виконати його очищення після завершення роботи. Для цього можна використовувати метод "Dispose()", який буде видаляти сформований файл. Для більш складної роботи з тимчасовими файлами, можна використовувати клас "System.IO.FileStream", який дозволяє встановлювати різні настройки і доступи до файлу.

##Дивись також

- [Клас "System.IO.Path" в MSDN](https://docs.microsoft.com/en-us/dotnet/api/system.io.path?view=netcore-3.1)
- [Клас "System.IO.FileStream" в MSDN](https://docs.microsoft.com/en-us/dotnet/api/system.io.filestream?view=netcore-3.1)
- [Стаття про тимчасові файли в C#](https://docs.microsoft.com/en-us/dotnet/standard/io/how-to-create-temporary-files)