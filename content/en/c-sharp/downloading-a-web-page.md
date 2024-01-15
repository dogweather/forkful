---
title:                "Downloading a web page"
html_title:           "C# recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Why

Sometimes, we may want to extract information from a web page or automate a task that requires us to download a specific web page. In such cases, knowing how to download a web page using C# can come in handy.

## How To

To download a web page using C#, we first need to use the `HttpClient` class from the `System.Net.Http` namespace. This class provides methods to make HTTP requests and retrieve responses from a web server. We can use the `GetAsync()` method to download a web page asynchronously and store the response in a `HttpResponseMessage` object.

````C#
using System;
using System.Net.Http;

public class Program
{
    public static async Task Main()
    {
        // Create an instance of HttpClient
        HttpClient client = new HttpClient();

        // Specify the URL of the web page to download
        string url = "https://www.example.com";

        // Make a GET request to the URL and retrieve the response
        HttpResponseMessage response = await client.GetAsync(url);

        // Check if the response is successful
        if (response.IsSuccessStatusCode)
        {
            // Get the response content as a string
            string content = await response.Content.ReadAsStringAsync();

            Console.WriteLine(content);
        }
    }
}
````
Output:
```html
<!DOCTYPE html>
<html>
<head>
    <title>Example Domain</title>
    <meta charset="utf-8" />
    <meta http-equiv="Content-type" content="text/html; charset=utf-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1" />
</head>
<body>
<div>
    <h1>Example Domain</h1>
    <p>This domain is for use in illustrative examples in documents. You may use this domain in literature without prior coordination or asking for permission.</p>
    <p><a href="https://www.iana.org/domains/example">More information...</a></p>
</div>
</body>
</html>
```

## Deep Dive

The `HttpClient` class supports various methods for making HTTP requests, such as `GetAsync()`, `PostAsync()`, `PutAsync()`, and `DeleteAsync()`, to name a few. These methods take the URL of the web page as a parameter and return a `Task<HttpResponseMessage>`, which can then be awaited to get the response.

The response from the `HttpClient` methods contains useful information, such as the status of the request, status code, headers, and content. We can use methods like `IsSuccessStatusCode` and `ReadAsStringAsync()` to check the status code and retrieve the response content, respectively.

It is also important to properly handle exceptions while downloading a web page. We can use a `try-catch` block to catch any exceptions that may occur, such as a `HttpRequestException` or `TaskCanceledException`, and handle them accordingly. We can also set a timeout for the request using the `Timeout` property of the `HttpClient` object.

## See Also

Here are some helpful resources for further reading on downloading a web page using C#:

- [HttpClient class - Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient)
- [Asynchronous programming with async and await - Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/concepts/async/)
- [Handling exceptions in C# - Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/exceptions/)