---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:57:37.010854-07:00
description: "\u0417\u0430\u0433\u0440\u0443\u0437\u043A\u0430 \u0432\u0435\u0431\
  -\u0441\u0442\u0440\u0430\u043D\u0438\u0446\u044B \u043E\u0437\u043D\u0430\u0447\
  \u0430\u0435\u0442 \u043F\u043E\u043B\u0443\u0447\u0435\u043D\u0438\u0435 \u0438\
  \u0441\u0445\u043E\u0434\u043D\u043E\u0433\u043E HTML-\u0441\u043E\u0434\u0435\u0440\
  \u0436\u0438\u043C\u043E\u0433\u043E \u0438\u0437 \u0438\u043D\u0442\u0435\u0440\
  \u043D\u0435\u0442\u0430 \u0441 \u043F\u043E\u043C\u043E\u0449\u044C\u044E \u043A\
  \u043E\u0434\u0430. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\u0441\
  \u0442\u044B \u0434\u0435\u043B\u0430\u044E\u0442 \u044D\u0442\u043E \u0434\u043B\
  \u044F \u043E\u0431\u0440\u0430\u0431\u043E\u0442\u043A\u0438 \u0434\u0430\u043D\
  \u043D\u044B\u0445,\u2026"
lastmod: '2024-03-13T22:44:45.053937-06:00'
model: gpt-4-0125-preview
summary: "\u0417\u0430\u0433\u0440\u0443\u0437\u043A\u0430 \u0432\u0435\u0431-\u0441\
  \u0442\u0440\u0430\u043D\u0438\u0446\u044B \u043E\u0437\u043D\u0430\u0447\u0430\u0435\
  \u0442 \u043F\u043E\u043B\u0443\u0447\u0435\u043D\u0438\u0435 \u0438\u0441\u0445\
  \u043E\u0434\u043D\u043E\u0433\u043E HTML-\u0441\u043E\u0434\u0435\u0440\u0436\u0438\
  \u043C\u043E\u0433\u043E \u0438\u0437 \u0438\u043D\u0442\u0435\u0440\u043D\u0435\
  \u0442\u0430 \u0441 \u043F\u043E\u043C\u043E\u0449\u044C\u044E \u043A\u043E\u0434\
  \u0430."
title: "\u0417\u0430\u0433\u0440\u0443\u0437\u043A\u0430 \u0432\u0435\u0431-\u0441\
  \u0442\u0440\u0430\u043D\u0438\u0446\u044B"
weight: 42
---

## Как:
C# упрощает загрузку веб-страницы с помощью класса `HttpClient`. Вот быстрый пример:

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
                string url = "http://example.com"; // Замените на желаемый URL
                HttpResponseMessage response = await client.GetAsync(url);
                response.EnsureSuccessStatusCode();
                string responseBody = await response.Content.ReadAsStringAsync();
                
                Console.WriteLine(responseBody); // Выводит исходное HTML-содержимое
            }
            catch (HttpRequestException e)
            {
                Console.WriteLine("\nИсключение Поймано!");
                Console.WriteLine("Сообщение: {0} ", e.Message);
            }
        }
    }
}
```

Это выведет HTML-содержимое указанной веб-страницы в консоль.

## Погружение в Детали
До `HttpClient`, C# использовал такие классы, как `WebClient` и `HttpWebRequest` для загрузки веб-содержимого. `HttpClient` - это последняя разработка и создан, чтобы быть многоразовым, эффективным и поддерживать асинхронные операции, что делает его предпочтительным выбором для новых приложений.

Существуют альтернативы. Например, сторонние библиотеки, такие как `HtmlAgilityPack`, могут анализировать HTML, что упрощает навигацию по DOM или извлечение конкретных фрагментов информации без работы с исходными HTML-строками.

При загрузке веб-страниц помните: уважайте файлы robots.txt, обрабатывайте исключения и будьте внимательны к условиям использования веб-сайтов.

## Смотрите Также
- [Документация класса HttpClient](https://docs.microsoft.com/ru-ru/dotnet/api/system.net.http.httpclient)
- [Async и Await](https://docs.microsoft.com/ru-ru/dotnet/csharp/programming-guide/concepts/async/)
- [HTML Agility Pack на GitHub](https://github.com/zzzprojects/html-agility-pack)
- [Соблюдение robots.txt](https://developers.google.com/search/docs/advanced/robots/intro)
