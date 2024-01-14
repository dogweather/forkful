---
title:                "C#: Перевірка наявності каталогу"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Чому

Перевірка існування директорії важлива для розробки програм, які потребують доступу до файлів з певної директорії, або для виявлення помилок при запуску програми.

## Як

У C#, існує простий спосіб перевірити існування директорії - метод "Directory.Exists()". Для цього потрібно передати шлях до директорії як аргумент, і метод поверне булеве значення: "true", якщо директорія існує і "false", якщо директорія не існує.

```C#
using System.IO;

string directoryPath = "C:\\Users\\User\\Documents\\MyDirectory";

bool exists = Directory.Exists(directoryPath);

Console.WriteLine("Does the directory exist? {0}", exists);

// Output: Does the directory exist? True
```

## Глибоке заглиблення

При перевірці існування директорії, важливо враховувати, що метод "Directory.Exists()" також буде повертати "true", якщо вказано шлях до файлу замість директорії. Тому перед використанням цього методу, необхідно перевірити, що це дійсно директорія, а не файл.

Крім того, метод "Directory.Exists()" перевіряє лише існування директорії і не враховує, чи користувач має доступ до цієї директорії. Тому, перед доступом до файлів з директорії, краще також перевірити права доступу за допомогою класу "DirectoryInfo" та методу "DirectoryInfo.GetAccessControl()".

## Дивись Також

- [Документація Microsoft про перевірку існування директорії в C#](https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.exists?view=net-5.0)
- [Приклад перевірки чи існує директорія в C#](https://www.tutlane.com/tutorial/csharp/csharp-check-directory-exists)
- [Тutorialspoint про перевірку директорії та її доступу в C#](https://www.tutorialspoint.com/check-if-a-directory-exists-in-c-sharp)