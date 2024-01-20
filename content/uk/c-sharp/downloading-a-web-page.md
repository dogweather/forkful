---
title:                "Завантаження веб-сторінки"
html_title:           "Gleam: Завантаження веб-сторінки"
simple_title:         "Завантаження веб-сторінки"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Що та для чого?

Завантаження веб-сторінки - це процес отримання даних веб-сторінки і запису їх на локальний пристрій. Програмісти це роблять, щоб аналізувати вміст веб-сторінки або використовувати контент веб-сайту в їх програмах.

## Як це зробити:

Ось приклад коду, що використовує HttpClient, щоб завантажити вміст веб-сторінки:

```C#
using System;
using System.Net.Http;
using System.Threading.Tasks;

class Program
{
    static readonly HttpClient client = new HttpClient();

    static async Task Main()
    {
        try
        {
            string responseBody = await client.GetStringAsync("http://www.example.com");

            Console.WriteLine(responseBody);
        }
        catch (HttpRequestException e)
        {
            Console.WriteLine("Error: {0}", e.Message);
        }
    }
}
```
Цей код у виводі надає HTML-код завантаженої веб-сторінки.

## Глибше занурення:

Подібно до створення HTTP-клієнта, бібліотеки, такі як HtmlAgilityPack або ScrapySharp, були створені щоб допомогти програмістам легко аналізувати вміст HTML. З тих пір, як було створено HttpClient в .NET 4.5, він став основою для встановлення HTTP-з'єднань в більшості програм C#.

В залежності від ваших потреб, є інші методи для завантаження веб-сторінок, такі як використання WebClient або HttpWebRequest.

HttpClient був створений з метою поліпшення процесу встановлення з'єднань за допомогою HTTP. Висока продуктивність і швидкість справили свою справу, і він став стандартом в програмуванні на C#.

## Див. також:

1. [MSDN HttpClient Class](https://docs.microsoft.com/dotnet/api/system.net.http.httpclient)
3. [ScrapySharp](https://github.com/rflechner/ScrapySharp)
4. [HtmlAgilityPack](https://html-agility-pack.net/)