---
title:                "Написання текстового файлу"
aliases: - /uk/c-sharp/writing-a-text-file.md
date:                  2024-02-03T19:27:45.142576-07:00
model:                 gpt-4-0125-preview
simple_title:         "Написання текстового файлу"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/writing-a-text-file.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?
Створення текстового файлу в C# включає програмне створення або модифікацію текстових файлів у файловій системі - фундаментальне завдання для багатьох додатків, таких як логування, експорт даних або управління конфігураціями. Програмісти виконують цю операцію, щоб зберегти дані між сесіями, обмінятися інформацією між системами або зберігати дані у людино-читабельному форматі.

## Як:
C# спрощує операції з файлами за допомогою простору імен `System.IO`, надаючи прямолінійні методи для запису текстових файлів. Ось як написати основний текстовий файл та додати текст до існуючого файлу.

### Написання текстового файлу з нуля
```csharp
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string filePath = @"C:\example\ExampleFile.txt";
        string content = "Привіт, світ!";

        // Записуємо вміст у новий файл
        File.WriteAllText(filePath, content);
        
        Console.WriteLine("Файл успішно записаний.");
    }
}
```
**Приклад виводу:**
```
Файл успішно записаний.
```

### Додавання тексту до існуючого файлу
Якщо ви бажаєте додати текст до кінця існуючого файлу, ви можете використати метод `File.AppendAllText`.

```csharp
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string filePath = @"C:\example\ExampleFile.txt";
        string additionalContent = "\nДодаємо більше вмісту.";

        // Додаємо вміст до файлу
        File.AppendAllText(filePath, additionalContent);
        
        Console.WriteLine("Вміст успішно додано.");
    }
}
```
**Приклад виводу:**
```
Вміст успішно додано.
```

### Використання сторонніх бібліотек: `StreamWriter`
Для більш детального контролю над записом, включаючи автоматичне оновлення та вибір кодування, використовуйте `StreamWriter`.

```csharp
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string filePath = @"C:\example\ExampleFile.txt";
        string content = "Це приклад використання StreamWriter.";

        // Використовуємо StreamWriter для запису у файл
        using (StreamWriter writer = new StreamWriter(filePath, append: true))
        {
            writer.WriteLine(content);
        }
        
        Console.WriteLine("Файл успішно записаний за допомогою StreamWriter.");
    }
}
```
**Приклад виводу:**
```
Файл успішно записаний за допомогою StreamWriter.
```

Кожен з цих підходів служить різним потребам: прямі методи `File` для швидких операцій і `StreamWriter` для складніших сценаріїв запису. Вибирайте залежно від своїх конкретних вимог, враховуючи такі фактори, як продуктивність та розмір файлу.
