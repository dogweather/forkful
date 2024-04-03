---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:02:33.964640-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: C# \u0443\u043F\u0440\u043E\u0449\u0430\u0435\u0442 \u043E\u0442\u043F\
  \u0440\u0430\u0432\u043A\u0443 HTTP-\u0437\u0430\u043F\u0440\u043E\u0441\u043E\u0432\
  \ \u0441 \u043F\u043E\u043C\u043E\u0449\u044C\u044E `HttpClient`. \u0412\u043E\u0442\
  \ \u043A\u0430\u0440\u043A\u0430\u0441 GET-\u0437\u0430\u043F\u0440\u043E\u0441\u0430\
  ."
lastmod: '2024-03-13T22:44:45.050539-06:00'
model: gpt-4-0125-preview
summary: "C# \u0443\u043F\u0440\u043E\u0449\u0430\u0435\u0442 \u043E\u0442\u043F\u0440\
  \u0430\u0432\u043A\u0443 HTTP-\u0437\u0430\u043F\u0440\u043E\u0441\u043E\u0432 \u0441\
  \ \u043F\u043E\u043C\u043E\u0449\u044C\u044E `HttpClient`."
title: "\u041E\u0442\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\u043F\u0440\
  \u043E\u0441\u0430"
weight: 44
---

## Как это сделать:
C# упрощает отправку HTTP-запросов с помощью `HttpClient`. Вот каркас GET-запроса:

```C#
using System;
using System.Net.Http;
using System.Threading.Tasks;

class Program
{
    static async Task Main(string[] args)
    {
        using HttpClient client = new HttpClient();
        HttpResponseMessage response = await client.GetAsync("http://example.com");
        response.EnsureSuccessStatusCode();
        string responseBody = await response.Content.ReadAsStringAsync();
        
        Console.WriteLine(responseBody);
    }
}
```

Пример вывода (усечен):
```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
```

## Глубокое погружение
`HttpClient` был введен в .NET Framework 4.5, чтобы упростить HTTP-общение. До этого скорее всего приходилось работать с классами `HttpWebRequest` и `HttpWebResponse`, которые были более громоздкими.

Есть и другие способы отправки HTTP-запросов в C#. `RestSharp` и `Flurl` это две популярные сторонние библиотеки, предлагающие более плавный интерфейс и дополнительные функции. Но `HttpClient` обычно более чем достаточен для большинства нужд.

С точки зрения реализации, `HttpClient` разработан для повторного использования для множества запросов. Создание его экземпляра для каждого запроса может исчерпать количество доступных сокетов при интенсивной загрузке. Всегда, и я подчеркиваю, всегда обращайте внимание на правильное уничтожение экземпляров `HttpClient`, чтобы избежать утечек ресурсов.

## См. также
- Документация по `HttpClient` от Microsoft: [https://docs.microsoft.com/ru-ru/dotnet/api/system.net.http.httpclient](https://docs.microsoft.com/ru-ru/dotnet/api/system.net.http.httpclient)
- Лучшие практики использования HttpClient: [https://aspnetmonsters.com/2016/08/2016-08-27-httpclientwrong/](https://aspnetmonsters.com/2016/08/2016-08-27-httpclientwrong/)
- Взаимодействие с RESTful API с `RestSharp`: [http://restsharp.org/](http://restsharp.org/)
- Работа с HTTP в стиле Fluent с `Flurl`: [https://flurl.dev/](https://flurl.dev/)
