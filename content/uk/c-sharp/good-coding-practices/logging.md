---
date: 2024-01-26 01:01:49.031443-07:00
description: "\u042F\u043A \u0446\u0435 \u0440\u043E\u0431\u0438\u0442\u0438: \u0423\
  \ C#, \u0432\u0438 \u043C\u043E\u0436\u0435\u0442\u0435 \u0432\u0438\u043A\u043E\
  \u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0432\u0430\u0442\u0438 \u0432\u0431\u0443\
  \u0434\u043E\u0432\u0430\u043D\u0438\u0439 \u043F\u0440\u043E\u0441\u0442\u0456\u0440\
  \ \u0456\u043C\u0435\u043D `System.Diagnostics` \u0430\u0431\u043E \u0441\u0442\u043E\
  \u0440\u043E\u043D\u043D\u0456 \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\
  \u0438, \u0442\u0430\u043A\u0456 \u044F\u043A NLog \u0430\u0431\u043E log4net. \u041E\
  \u0441\u044C \u0448\u0432\u0438\u0434\u043A\u0438\u0439\u2026"
lastmod: '2024-03-13T22:44:49.300157-06:00'
model: gpt-4-1106-preview
summary: "\u0423 C#, \u0432\u0438 \u043C\u043E\u0436\u0435\u0442\u0435 \u0432\u0438\
  \u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0432\u0430\u0442\u0438 \u0432\
  \u0431\u0443\u0434\u043E\u0432\u0430\u043D\u0438\u0439 \u043F\u0440\u043E\u0441\u0442\
  \u0456\u0440 \u0456\u043C\u0435\u043D `System.Diagnostics` \u0430\u0431\u043E \u0441\
  \u0442\u043E\u0440\u043E\u043D\u043D\u0456 \u0431\u0456\u0431\u043B\u0456\u043E\u0442\
  \u0435\u043A\u0438, \u0442\u0430\u043A\u0456 \u044F\u043A NLog \u0430\u0431\u043E\
  \ log4net."
title: "\u041B\u043E\u0433\u0443\u0432\u0430\u043D\u043D\u044F"
weight: 17
---

## Як це робити:
У C#, ви можете використовувати вбудований простір імен `System.Diagnostics` або сторонні бібліотеки, такі як NLog або log4net. Ось швидкий приклад використання інтерфейсу `ILogger`, доступного у .NET Core:

```C#
using Microsoft.Extensions.Logging;
using System;

public class Program
{
    public static void Main()
    {
        using var loggerFactory = LoggerFactory.Create(builder => {
            builder.AddConsole();
        });

        ILogger logger = loggerFactory.CreateLogger<Program>();

        logger.LogInformation("Це інформаційне повідомлення.");
        logger.LogWarning("Це попереджувальне повідомлення.");
        logger.LogError("Це повідомлення про помилку.");
    }
}
```

Приклад виводу:
```
info: Program[0]
      Це інформаційне повідомлення.
warn: Program[0]
      Це попереджувальне повідомлення.
fail: Program[0]
      Це повідомлення про помилку.
```

## Поглиблено
Історія логування у розробці програмного забезпечення майже така стара, як і саме програмування; вона еволюціонувала від простих вказівок друку до складних, налаштованих систем. Історично логування виконувалося шляхом запису до файлів чи консолі, але це розширилося до більш складних структур, таких як системи агрегації логів і платформи розподіленого трасування (як ELK стек чи Jaeger).

Альтернативи вбудованому логуванню в .NET включають сторонні бібліотеки:
- **NLog**: універсальна і легка в налаштуванні, з багатьма можливостями для маршрутизації, форматування і фільтрації логів.
- **log4net**: натхненний бібліотекою Java log4j, він високо налаштовується з XML і підтримує різноманітні репозиторії логів.

Коли мова йде про деталі реалізації, вибір вашої абстракції логування (як Microsoft.Extensions.Logging) та базового провайдера логування може значно вплинути на продуктивність та надійність додатка. Дуже важливо правильно налаштувати рівні логування і переконатися, що запис логів не стане вузьким місцем. 

Також, структуроване логування - коли ви логуєте не просто рядки, а пари ключ-значення або об'єкти - дозволяє отримати більш точні і дієві логи, які легше запитувати та аналізувати.

## Дивіться також
- [Документація Microsoft.Extensions.Logging](https://docs.microsoft.com/en-us/aspnet/core/fundamentals/logging/)
- [Документація NLog](https://nlog-project.org/documentation/)
- [Документація log4net](https://logging.apache.org/log4net/)
- [Документація Serilog](https://serilog.net/) (приклад структурованого логування)
