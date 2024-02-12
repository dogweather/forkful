---
title:                "Inviare una richiesta http"
aliases: - /it/c-sharp/sending-an-http-request.md
date:                  2024-01-20T17:59:08.646234-07:00
model:                 gpt-4-1106-preview
simple_title:         "Inviare una richiesta http"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa e perché?)
Mandare una richiesta HTTP significa chiedere dati o servizi a un server web. Programmatori lo fanno per interagire con API, scaricare contenuti o inviare form online.

## How to: (Come fare:)
```C#
using System;
using System.Net.Http;
using System.Threading.Tasks;

class Program
{
    static async Task Main(string[] args)
    {
        using var client = new HttpClient();
        
        // GET request
        HttpResponseMessage response = await client.GetAsync("http://example.com/api/data");
        if (response.IsSuccessStatusCode)
        {
            string data = await response.Content.ReadAsStringAsync();
            Console.WriteLine(data);
        }

        // POST request
        var values = new Dictionary<string, string>
        {
            { "key1", "value1" },
            { "key2", "value2" }
        };

        HttpContent content = new FormUrlEncodedContent(values);
        response = await client.PostAsync("http://example.com/api/post", content);
        string responseString = await response.Content.ReadAsStringAsync();
        Console.WriteLine(responseString);
    }
}
```
Output di esempio:
```
{"id": 1, "name": "Mario Rossi"}
{"result": "success"}
```

## Deep Dive (Approfondimento)
Inviare richieste HTTP è fondamentale da quando il web si è evoluto da semplici siti statici a applicazioni complesse. Il `HttpClient` in C# è la risorsa dal .NET Framework 4.5. Alternativamente, si usavano `WebRequest` e `WebClient`, ma `HttpClient` supera per flessibilità e performance. `Async-Await` è essenziale per non bloccare l'UI durante le richieste. Devi gestire le eccezioni e assicurarti di disporre di connessioni sicure in `HTTPS`.

## See Also (Vedi Anche)
- Documentazione ufficiale `HttpClient` [qui](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient?view=net-6.0)
- Best practices su come utilizzare `HttpClient` [qui](https://docs.microsoft.com/en-us/dotnet/csharp/tutorials/console-webapiclient)
- Introduzione a REST e API web in ASP.NET Core [qui](https://docs.microsoft.com/en-us/aspnet/core/web-api/?view=aspnetcore-6.0)
