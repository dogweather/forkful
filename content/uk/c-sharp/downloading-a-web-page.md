---
title:                "Завантаження веб-сторінки"
html_title:           "C#: Завантаження веб-сторінки"
simple_title:         "Завантаження веб-сторінки"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Що & Чому?
Завантаження веб-сторінки - це коли програміст збирає дані з інтернет-адреси і зберігає їх у програмі. Програмісти роблять це, щоб отримати інформацію, яка буде використовуватися для різних цілей, таких як аналіз чи обробка.

## Як зробити:
```C#
using System.Net;

// створюємо об'єкт WebClient
WebClient client = new WebClient();

// задаємо URL адресу для завантаження
string url = "https://example.com";

// використовуємо метод DownloadString для завантаження сторінки
string content = client.DownloadString(url);

// виводимо зміст сторінки у консоль
Console.WriteLine(content);
```

## Більш глибоке погруження:
Завантаження веб-сторінки стало можливим завдяки поширенню інтернету та розвитку технологій. Проте, є інші способи отримати дані з веб-сторінок, такі як парсинг HTML-коду або використання API. У C# також існує багато бібліотек для завантаження веб-сторінок, які надають більш розширені можливості.

## Дивись також:
- [Документація Microsoft про клас WebClient](https://docs.microsoft.com/en-us/dotnet/api/system.net.webclient?view=net-5.0)
- [Стаття про завантаження веб-сторінок в C#](https://www.c-sharpcorner.com/blogs/download-a-webpage-in-html-format-in-net-core)
- [Бібліотека AngleSharp для парсингу HTML-коду в C#](https://anglesharp.github.io/)