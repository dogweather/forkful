---
date: 2024-01-20 17:59:33.444796-07:00
description: "How to: C# g\xF6r det enkelt att skicka HTTP-requests. Med `HttpClient`\
  \ kan vi h\xE4mta en webbsida med bara n\xE5gra rader kod."
lastmod: '2024-03-13T22:44:37.909947-06:00'
model: gpt-4-1106-preview
summary: "C# g\xF6r det enkelt att skicka HTTP-requests."
title: "Skicka en http-f\xF6rfr\xE5gan"
weight: 44
---

## How to:
C# gör det enkelt att skicka HTTP-requests. Med `HttpClient` kan vi hämta en webbsida med bara några rader kod.

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

Kör programmet, och om allt går som det ska, kommer du se webbsidans HTML i konsolen.

## Deep Dive
För att förstå HTTP-requests i C# behöver vi blicka bakåt. `HttpClient` introducerades i .NET Framework 4.5 och blev snabbt standard för att hantera HTTP-kommunikation i C#. Alternativ som `WebRequest` och `WebClient` är äldre och mindre flexibla.

`HttpClient` stödjer asynkrona anrop, vilket innebär bättre prestanda i I/O-bundna operationer. Den låter oss också anpassa headers, timeout-tider och hantera cookies, vilket är essentiellt för moderna webbapplikationer.

Användning av `using` är nyckeln här. Det ser till att `HttpClient` frigörs på rätt sätt och undviker problem med socket-utarmning som äldre metoder led av.

## See Also
- Microsoft dokumentation för `HttpClient`: [docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient)
- Artikel om att hantera exceptions i HTTP-anrop: [docs.microsoft.com/en-us/dotnet/csharp/programming-guide/exceptions](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/exceptions)
- REST API-guiden för utvecklare: [restapitutorial.com](http://www.restapitutorial.com)
