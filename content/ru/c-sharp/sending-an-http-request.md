---
title:                "Отправка HTTP-запроса"
date:                  2024-01-29T00:02:33.964640-07:00
model:                 gpt-4-0125-preview
simple_title:         "Отправка HTTP-запроса"

category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/c-sharp/sending-an-http-request.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?
Отправка HTTP-запроса - это способ общения программ через веб, запрашивая данные или отправляя их. Программисты делают это для взаимодействия с API, сервисами или для получения веб-контента.

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
