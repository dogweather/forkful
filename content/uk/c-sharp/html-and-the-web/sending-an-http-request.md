---
aliases:
- /uk/c-sharp/sending-an-http-request/
date: 2024-01-20 17:59:30.128089-07:00
description: "Sending an HTTP request means asking another system for data or a response\
  \ over the web. Programmers do it to interact with APIs, fetch web content, or\u2026"
lastmod: 2024-02-18 23:09:00.335330
model: gpt-4-1106-preview
summary: "Sending an HTTP request means asking another system for data or a response\
  \ over the web. Programmers do it to interact with APIs, fetch web content, or\u2026"
title: "\u041D\u0430\u0434\u0441\u0438\u043B\u0430\u043D\u043D\u044F HTTP-\u0437\u0430\
  \u043F\u0438\u0442\u0443"
---

{{< edit_this_page >}}

## What & Why? (Що і Чому?)
Sending an HTTP request means asking another system for data or a response over the web. Programmers do it to interact with APIs, fetch web content, or communicate between services. 

## How to: (Як зробити:)
```C#
using System;
using System.Net.Http;
using System.Threading.Tasks;

class Program
{
    static async Task Main(string[] args)
    {
        using (var client = new HttpClient())
        {
            try
            {
                // Sending a GET request to example.com
                HttpResponseMessage response = await client.GetAsync("http://example.com");
                response.EnsureSuccessStatusCode();
                string responseBody = await response.Content.ReadAsStringAsync();
                
                Console.WriteLine(responseBody);  // Outputs the retrieved HTML
            }
            catch(HttpRequestException e)
            {
                Console.WriteLine("\nException Caught!");
                Console.WriteLine("Message: {0} ", e.Message);
            }
        }
    }
}
```
Sample output:
```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
    ...
</head>
<body>
    ...
</body>
</html>
```

## Deep Dive (Поглиблене Вивчення):
HTTP requests are foundational to web communication, established by Tim Berners-Lee in 1989. Alternatives like gRPC or WebSockets are used when you need real-time communication or other protocols.

Implementing an HTTP request in C# is straightforward thanks to the `HttpClient` class. Released with .NET Framework 4.5, `HttpClient` provides an async API for sending HTTP requests. Keep in mind to dispose of `HttpClient` instances to avoid resource leaks, or better yet, use a single instance throughout your app.

Another thing to consider is handling exceptions. With proper exception handling, your app can gracefully deal with network issues or bad responses.

In older C# versions, developers had to use `WebRequest` and `WebResponse` classes which were more cumbersome. `HttpClient` is not only easier to use but also supports modern async programming paradigms.

## See Also (Дивіться також):
- [HttpClient Class Documentation](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient)
- [Understanding HTTP](https://developer.mozilla.org/en-US/docs/Web/HTTP)
- [API Communication with REST](https://restfulapi.net/)
- [Alternatives to HTTP: gRPC](https://grpc.io/)
- [Real-time Web Communication: WebSockets](https://developer.mozilla.org/en-US/docs/Web/API/WebSockets_API)
