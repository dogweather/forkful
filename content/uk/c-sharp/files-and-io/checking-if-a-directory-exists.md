---
title:                "Перевірка наявності директорії"
aliases:
- /uk/c-sharp/checking-if-a-directory-exists/
date:                  2024-02-03T19:08:22.853851-07:00
model:                 gpt-4-0125-preview
simple_title:         "Перевірка наявності директорії"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і Чому?

Перевірка наявності директорії в C# полягає у верифікації присутності папки за вказаним шляхом у файловій системі. Програмісти роблять це, щоб уникнути помилок, таких як спроби читання з або запису в неіснуючу директорію, забезпечуючи більш гладке маніпулювання файлами та директоріями.

## Як це зробити:

### Використовуючи System.IO

C# надає простір імен `System.IO`, який містить клас `Directory`, що пропонує прямий спосіб перевірки існування директорії через метод `Exists`.

```csharp
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string directoryPath = @"C:\ExampleDirectory";

        // Перевіряємо, чи існує директорія
        bool directoryExists = Directory.Exists(directoryPath);

        // Виводимо результат
        Console.WriteLine("Directory exists: " + directoryExists);
    }
}
```

**Приклад виводу:**

```
Directory exists: False
```

У випадку, якщо директорія дійсно існує за шляхом `C:\ExampleDirectory`, вивід буде `True`.

### Використання System.IO.Abstractions для модульного тестування

Коли справа доходить до того, щоб зробити ваш код придатним для модульного тестування, особливо коли він взаємодіє з файловою системою, пакет `System.IO.Abstractions` є популярним вибором. Він дозволяє вам абстрагуватися та імітувати операції з файловою системою у ваших тестах. Ось як ви могли б перевірити існування директорії, використовуючи цей підхід:

Спочатку, переконайтеся, що ви встановили пакет:

```
Install-Package System.IO.Abstractions
```

Після цього, ви можете інжектити `IFileSystem` у ваш клас та використовувати його для перевірки існування директорії, що дозволяє легше проводити модульне тестування.

```csharp
using System;
using System.IO.Abstractions;

class Program
{
    private readonly IFileSystem _fileSystem;

    public Program(IFileSystem fileSystem)
    {
        _fileSystem = fileSystem;
    }

    public bool CheckDirectoryExists(string directoryPath)
    {
        return _fileSystem.Directory.Exists(directoryPath);
    }

    static void Main()
    {
        var fileSystem = new FileSystem();
        var program = new Program(fileSystem);

        string directoryPath = @"C:\ExampleDirectory";
        bool directoryExists = program.CheckDirectoryExists(directoryPath);

        Console.WriteLine("Directory exists: " + directoryExists);
    }
}
```

**Приклад виводу:**

```
Directory exists: False
```

Цей підхід розв'язує вашу програмну логіку від прямого доступу до файлової системи, роблячи ваш код більш модульним, придатним для тестування та підтримуваним.
