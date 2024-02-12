---
title:                "Scaricare una pagina web"
aliases:
- /it/c-sharp/downloading-a-web-page/
date:                  2024-01-20T17:43:42.256294-07:00
model:                 gpt-4-1106-preview
simple_title:         "Scaricare una pagina web"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa e Perché?)
Scaricare una pagina web significa prelevare il suo contenuto via HTTP o HTTPS. I programmatori lo fanno per analizzare dati, verificare disponibilità o integrare informazioni in app.

## How to: (Come Fare:)
In C# si può usare HttpClient. Ecco come:

```C#
using System;
using System.Net.Http;
using System.Threading.Tasks;

class Program
{
    static async Task Main(string[] args)
    {
        // HttpClient è riciclato per l'efficienza
        using (var httpClient = new HttpClient())
        {
            // GetAsync per scaricare il contenuto
            HttpResponseMessage response = await httpClient.GetAsync("http://example.com");
            
            if (response.IsSuccessStatusCode)
            {
                string content = await response.Content.ReadAsStringAsync();
                Console.WriteLine(content); // Stampa il contenuto della pagina
            }
            else
            {
                Console.WriteLine("Errore: " + response.StatusCode);
            }
        }
    }
}
```

Questo stamperà il codice HTML della pagina http://example.com o un messaggio d'errore.

## Deep Dive (Approfondimento)
Prima dello standard HttpClient, usavamo WebRequest e WebClient. HttpClient è più semplice e gestisce meglio le connessioni. 

Oltre a GetAsync, esistono altri metodi come PostAsync e PutAsync per interagire con i servizi web RESTful. 

HttpClient dovrebbe essere istanziato una volta e riutilizzato, evitando sprechi di risorse. Implementazioni come IHttpClientFactory in ASP.NET Core ne migliorano ancora l'uso e la gestione.

## See Also (Vedi Anche)
- Documentazione ufficiale di HttpClient: [docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient)
- Guida a IHttpClientFactory in ASP.NET Core: [docs.microsoft.com/en-us/aspnet/core/fundamentals/http-requests](https://docs.microsoft.com/en-us/aspnet/core/fundamentals/http-requests)
- Approfondimenti HTTP in C#: [codeproject.com](https://www.codeproject.com/Articles/1256597/HTTP-Requests-in-Csharp)
