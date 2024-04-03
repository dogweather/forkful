---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:45.142576-07:00
description: "\u042F\u043A: C# \u0441\u043F\u0440\u043E\u0449\u0443\u0454 \u043E\u043F\
  \u0435\u0440\u0430\u0446\u0456\u0457 \u0437 \u0444\u0430\u0439\u043B\u0430\u043C\
  \u0438 \u0437\u0430 \u0434\u043E\u043F\u043E\u043C\u043E\u0433\u043E\u044E \u043F\
  \u0440\u043E\u0441\u0442\u043E\u0440\u0443 \u0456\u043C\u0435\u043D `System.IO`,\
  \ \u043D\u0430\u0434\u0430\u044E\u0447\u0438 \u043F\u0440\u044F\u043C\u043E\u043B\
  \u0456\u043D\u0456\u0439\u043D\u0456 \u043C\u0435\u0442\u043E\u0434\u0438 \u0434\
  \u043B\u044F \u0437\u0430\u043F\u0438\u0441\u0443 \u0442\u0435\u043A\u0441\u0442\
  \u043E\u0432\u0438\u0445 \u0444\u0430\u0439\u043B\u0456\u0432. \u041E\u0441\u044C\
  \ \u044F\u043A \u043D\u0430\u043F\u0438\u0441\u0430\u0442\u0438\u2026"
lastmod: '2024-03-13T22:44:49.318409-06:00'
model: gpt-4-0125-preview
summary: "C# \u0441\u043F\u0440\u043E\u0449\u0443\u0454 \u043E\u043F\u0435\u0440\u0430\
  \u0446\u0456\u0457 \u0437 \u0444\u0430\u0439\u043B\u0430\u043C\u0438 \u0437\u0430\
  \ \u0434\u043E\u043F\u043E\u043C\u043E\u0433\u043E\u044E \u043F\u0440\u043E\u0441\
  \u0442\u043E\u0440\u0443 \u0456\u043C\u0435\u043D `System.IO`, \u043D\u0430\u0434\
  \u0430\u044E\u0447\u0438 \u043F\u0440\u044F\u043C\u043E\u043B\u0456\u043D\u0456\u0439\
  \u043D\u0456 \u043C\u0435\u0442\u043E\u0434\u0438 \u0434\u043B\u044F \u0437\u0430\
  \u043F\u0438\u0441\u0443 \u0442\u0435\u043A\u0441\u0442\u043E\u0432\u0438\u0445\
  \ \u0444\u0430\u0439\u043B\u0456\u0432."
title: "\u041D\u0430\u043F\u0438\u0441\u0430\u043D\u043D\u044F \u0442\u0435\u043A\u0441\
  \u0442\u043E\u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0443"
weight: 24
---

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
