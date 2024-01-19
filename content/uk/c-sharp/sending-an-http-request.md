---
title:                "Надсилання http-запиту"
html_title:           "Arduino: Надсилання http-запиту"
simple_title:         "Надсилання http-запиту"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Що і чому?

Надсилання HTTP-запиту - це процес комунікації між комп'ютерами через протокол HTTP, коли один комп'ютер запитує дані від іншого. Ми, програмісти, робимо це, щоб отримати доступ до різноманітних веб-ресурсів, наприклад, веб-сторінок, API, файлів тощо.

## Як це робиться:

Подивимося на приклад коду, який виконує HTTP-запит за допомогою C# і HttpClient.

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
            HttpResponseMessage response = await client.GetAsync("http://example.com");
            response.EnsureSuccessStatusCode();
            string responseBody = await response.Content.ReadAsStringAsync();
            Console.WriteLine(responseBody);
        }  
        catch(HttpRequestException e)
        {  
            Console.WriteLine("Exception Caught!");	
            Console.WriteLine($"Message: {e.Message} ");
        }
    }
}
```
При виконанні цього коду отримуємо HTML-текст веб-сторінки "http://example.com" або повідомлення про помилку, якщо запит не вдався.

## Більш глибокий занурення

Надсилання HTTP-запитів було основним методом взаємодії в Інтернеті з часів його зародження. Спочатку воно використовувалося для отримання HTML-документів, але з часом було розширено для взаємодії із серверними API.

Щодо альтернатив, можна користуватися іншими HTTP-клієнтами, як-от RestSharp або Flurl, які мають свої переваги і недоліки.

Метод GetAsync() використовує асинхронну модель програмування Task-based, що була введена в .NET 4.5. Вона робить код більш ефективним і простим у використанні.

## Див. також

- Офіційна документація про HttpClient: <https://docs.microsoft.com/uk-ua/dotnet/api/system.net.http.httpclient>
- Матеріал про асинхронне програмування в .NET: <https://docs.microsoft.com/uk-ua/dotnet/csharp/programming-guide/concepts/async/>
- RestSharp: <https://restsharp.dev/>
- Flurl: <https://flurl.dev/>