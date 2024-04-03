---
date: 2024-01-20 17:44:13.096766-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u041E\u0442\u0436\u0435, \u0432\u0430\u043C \u0442\u0440\u0435\u0431\u0430 \u0437\
  \u0430\u0432\u0430\u043D\u0442\u0430\u0436\u0438\u0442\u0438 \u0432\u0435\u0431\
  -\u0441\u0442\u043E\u0440\u0456\u043D\u043A\u0443 \u0432 C#. \u0414\u0430\u0432\u0430\
  \u0439\u0442\u0435 \u0441\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u0454\u043C\u043E\
  \u0441\u044F \u043A\u043B\u0430\u0441\u043E\u043C `HttpClient`. \u0426\u0435 \u043F\
  \u0440\u043E\u0441\u0442\u0438\u0439 \u043F\u0440\u0438\u043A\u043B\u0430\u0434."
lastmod: '2024-03-13T22:44:49.286884-06:00'
model: gpt-4-1106-preview
summary: "\u041E\u0442\u0436\u0435, \u0432\u0430\u043C \u0442\u0440\u0435\u0431\u0430\
  \ \u0437\u0430\u0432\u0430\u043D\u0442\u0430\u0436\u0438\u0442\u0438 \u0432\u0435\
  \u0431-\u0441\u0442\u043E\u0440\u0456\u043D\u043A\u0443 \u0432 C#."
title: "\u0417\u0430\u0432\u0430\u043D\u0442\u0430\u0436\u0435\u043D\u043D\u044F \u0432\
  \u0435\u0431-\u0441\u0442\u043E\u0440\u0456\u043D\u043A\u0438"
weight: 42
---

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
