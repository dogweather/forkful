---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:22.853851-07:00
description: "\u041F\u0435\u0440\u0435\u0432\u0456\u0440\u043A\u0430 \u043D\u0430\u044F\
  \u0432\u043D\u043E\u0441\u0442\u0456 \u0434\u0438\u0440\u0435\u043A\u0442\u043E\u0440\
  \u0456\u0457 \u0432 C# \u043F\u043E\u043B\u044F\u0433\u0430\u0454 \u0443 \u0432\u0435\
  \u0440\u0438\u0444\u0456\u043A\u0430\u0446\u0456\u0457 \u043F\u0440\u0438\u0441\u0443\
  \u0442\u043D\u043E\u0441\u0442\u0456 \u043F\u0430\u043F\u043A\u0438 \u0437\u0430\
  \ \u0432\u043A\u0430\u0437\u0430\u043D\u0438\u043C \u0448\u043B\u044F\u0445\u043E\
  \u043C \u0443 \u0444\u0430\u0439\u043B\u043E\u0432\u0456\u0439 \u0441\u0438\u0441\
  \u0442\u0435\u043C\u0456. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\
  \u0442\u0438 \u0440\u043E\u0431\u043B\u044F\u0442\u044C \u0446\u0435, \u0449\u043E\
  \u0431 \u0443\u043D\u0438\u043A\u043D\u0443\u0442\u0438\u2026"
lastmod: '2024-03-13T22:44:49.312172-06:00'
model: gpt-4-0125-preview
summary: "\u041F\u0435\u0440\u0435\u0432\u0456\u0440\u043A\u0430 \u043D\u0430\u044F\
  \u0432\u043D\u043E\u0441\u0442\u0456 \u0434\u0438\u0440\u0435\u043A\u0442\u043E\u0440\
  \u0456\u0457 \u0432 C# \u043F\u043E\u043B\u044F\u0433\u0430\u0454 \u0443 \u0432\u0435\
  \u0440\u0438\u0444\u0456\u043A\u0430\u0446\u0456\u0457 \u043F\u0440\u0438\u0441\u0443\
  \u0442\u043D\u043E\u0441\u0442\u0456 \u043F\u0430\u043F\u043A\u0438 \u0437\u0430\
  \ \u0432\u043A\u0430\u0437\u0430\u043D\u0438\u043C \u0448\u043B\u044F\u0445\u043E\
  \u043C \u0443 \u0444\u0430\u0439\u043B\u043E\u0432\u0456\u0439 \u0441\u0438\u0441\
  \u0442\u0435\u043C\u0456. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\
  \u0442\u0438 \u0440\u043E\u0431\u043B\u044F\u0442\u044C \u0446\u0435, \u0449\u043E\
  \u0431 \u0443\u043D\u0438\u043A\u043D\u0443\u0442\u0438\u2026"
title: "\u041F\u0435\u0440\u0435\u0432\u0456\u0440\u043A\u0430 \u043D\u0430\u044F\u0432\
  \u043D\u043E\u0441\u0442\u0456 \u0434\u0438\u0440\u0435\u043A\u0442\u043E\u0440\u0456\
  \u0457"
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
