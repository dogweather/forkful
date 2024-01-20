---
title:                "Sending an http request"
html_title:           "Bash recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Sending HTTP Requests in C#

## What & Why?
HTTP requests play a crucial role in client-server communication, enabling the client to ask for data from the server. Programmers use HTTP requests to fetch data from APIs, webpages, or any URLs.

## How to:
C# provides the `HttpClient` class to send HTTP requests. Here's how to use it to send a GET request, and print the data received from a URL:

```
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
            HttpResponseMessage response = await client.GetAsync("http://www.example.com");
            response.EnsureSuccessStatusCode();
            string responseBody = await response.Content.ReadAsStringAsync();
            Console.WriteLine(responseBody);
        }  
        catch(HttpRequestException e)
        {
            Console.WriteLine("\nException Caught!");	
            Console.WriteLine("Message :{0} ",e.Message);
        }
    }
}
```

Output:
``` 
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...omitted for brevity...
</html>
```

## Deep Dive
The `HttpClient` class used above was first introduced in .NET Framework 4.5 to overcome the limitations of `HttpWebRequest`, such as reusable connections, streaming, and pipeline requests.

While alternatives like `HttpWebRequest` and `WebClient` exist, `HttpClient` is recommended due to its superiority in terms of usability and performance. 

When you instantiate `HttpClient`, it opens a socket that stays open until it's manually closed or a timeout occurs, making `HttpClient` ideal for lengthy tasks or when many requests are sent to the same server. 

## See Also
1. [Microsoft Docs: HttpClient Class](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient)
3. [HttpClient vs HttpWebRequest](https://www.infoworld.com/article/2993352/httpclient-vs-httpwebrequest-dont-use-httpwebrequest.html)