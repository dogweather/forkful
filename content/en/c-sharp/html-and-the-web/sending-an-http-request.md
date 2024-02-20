---
date: 2024-01-20 17:59:13.483723-07:00
description: "Sending an HTTP request is a way for programs to communicate over the\
  \ web, asking for data or submitting some. Programmers do it to interact with APIs,\u2026"
lastmod: 2024-02-19 22:05:18.551323
model: gpt-4-1106-preview
summary: "Sending an HTTP request is a way for programs to communicate over the web,\
  \ asking for data or submitting some. Programmers do it to interact with APIs,\u2026"
title: Sending an HTTP request
---

{{< edit_this_page >}}

## What & Why?
Sending an HTTP request is a way for programs to communicate over the web, asking for data or submitting some. Programmers do it to interact with APIs, services or to pull in web content.

## How to:
C# makes sending HTTP requests straightforward with `HttpClient`. Here's the skeleton of a GET request:

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

Sample output (truncated):
```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
```

## Deep Dive
`HttpClient` was introduced in .NET Framework 4.5 to make HTTP communication easier. Before that, you'd likely have to wrestle with `HttpWebRequest` and `HttpWebResponse` classes, which were more cumbersome.

There are other ways to send HTTP requests in C#. `RestSharp` and `Flurl` are two popular third-party libraries offering a more fluent interface and extra features. But `HttpClient` is usually more than enough for most needs.

Implementation wise, `HttpClient` is designed to be reused for multiple requests. Instantiating it for each request can exhaust the number of sockets available under heavy loads. Always, and I mean always, pay attention to proper disposal of `HttpClient` instances to avoid resource leaks.

## See Also
- Microsoft's `HttpClient` documentation: [https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient)
- HttpClient best practices: [https://aspnetmonsters.com/2016/08/2016-08-27-httpclientwrong/](https://aspnetmonsters.com/2016/08/2016-08-27-httpclientwrong/)
- RESTful API interaction with `RestSharp`: [http://restsharp.org/](http://restsharp.org/)
- Fluent HTTP (HTTP made fluent) with `Flurl`: [https://flurl.dev/](https://flurl.dev/)
