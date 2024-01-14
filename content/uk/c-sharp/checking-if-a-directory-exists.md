---
title:                "C#: Перевірка існування папки"
simple_title:         "Перевірка існування папки"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Почему

Існування директорії може бути важливим кроком при роботі з файловою системою у вашому C# програмі. Це дозволяє перевірити наявність певної директорії перед тим, як продовжувати виконання програми.

## Як

Для перевірки існування директорії у C#, використовується метод `Directory.Exists()`. Нижче наведений приклад коду, який перевіряє існування директорії "Documents" на робочому столі.

```C#
string path = Environment.GetFolderPath(Environment.SpecialFolder.Desktop) + "\\Documents";
if (Directory.Exists(path))
{
    Console.WriteLine("Директорія Documents існує.");
}
else
{
    Console.WriteLine("Директорія Documents не існує.");
}
```

Результатом такої програми буде виведення повідомлення "Директорія Documents існує." якщо директорія дійсно існує на вашому робочому столі. Інакше буде виведено повідомлення "Директорія Documents не існує."

## Глибший аналіз

Під час перевірки існування директорії, метод `Directory.Exists()` шукає директорію за встановленим шляхом і повертає значення типу `bool` (true або false) в залежності від того, чи була директорія знайдена. Цей метод дуже корисний, коли ви маєте справу з багатьма директоріями і потребуєте впевнитися, що вони всі існують перед продовженням виконання програми.

## Дивіться також

- [Як створити директорію у C#](https://docs.microsoft.com/en-us/dotnet/standard/io/how-to-create-a-directory)
- [Як перевірити, чи існує файл у C#](https://docs.microsoft.com/en-us/dotnet/standard/io/how-to-check-if-a-file-exists)
- [Основи роботи з файлами та директоріями в C#](https://www.c-sharpcorner.com/UploadFile/mahesh/filesdirectoriesincsharp11262005035423AM/filesdirectoriesincsharp.aspx)