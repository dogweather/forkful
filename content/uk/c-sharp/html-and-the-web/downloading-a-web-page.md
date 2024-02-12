---
title:                "Завантаження веб-сторінки"
aliases: - /uk/c-sharp/downloading-a-web-page.md
date:                  2024-01-20T17:44:13.096766-07:00
model:                 gpt-4-1106-preview
simple_title:         "Завантаження веб-сторінки"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Що це таке та навіщо?
Завантаження веб-сторінки — це процес отримання її даних через інтернет. Програмісти це роблять, аби автоматизувати збір інформації, тестувати веб-служби чи створювати кеші сторінок.

## Як це зробити:
Отже, вам треба завантажити веб-сторінку в C#. Давайте скористаємося класом `HttpClient`. Це простий приклад:

```C#
using System;
using System.Net.Http;
using System.Threading.Tasks;

class Program
{
    static async Task Main(string[] args)
    {
        using (HttpClient client = new HttpClient())
        {
            try
            {
                string url = "http://example.com";
                HttpResponseMessage response = await client.GetAsync(url);
                response.EnsureSuccessStatusCode();
                string responseBody = await response.Content.ReadAsStringAsync();
                
                Console.WriteLine(responseBody);
            }
            catch (HttpRequestException e)
            {
                Console.WriteLine("\nException Caught!");
                Console.WriteLine("Message :{0} ", e.Message);
            }
        }
    }
}
```
Після запуску ви отримаєте HTML веб-сторінки на вашу консоль.

## Пірнання на глибину:
У минулому програмісти часто використовували `WebClient` або `HttpWebRequest` для завантаження веб-сторінок, але `HttpClient` став кращим вибором через свою ефективність і простоту у використанні. `HttpClient` дозволяє легко обробляти запити і відповіді JSON, що робить його ідеальним для спілкування з REST API.

Є альтернативи, наприклад, сторонні бібліотеки, як `RestSharp` чи `Flurl`, які надають додаткові функції і більш простий синтаксис. Тим не менш, для більшості базових завдань `HttpClient` - це все, що вам потрібно.

Ключеві моменти роботи з `HttpClient` включають управління життєвим циклом об'єкта, правильне використання асинхронного програмування для запобігання блокуванню потоків і використання `HttpResponseMessage` для перевірки результатів запитів.

## Ось також:
- [HttpClient Class Documentation](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient)
- [Async Programming in C#](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/concepts/async/)
- [`HttpResponseMessage` Class](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpresponsemessage)
- [RestSharp GitHub](https://github.com/restsharp/RestSharp)
- [Flurl GitHub](https://github.com/tmenier/Flurl)
