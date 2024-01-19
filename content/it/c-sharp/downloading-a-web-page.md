---
title:                "Scaricare una pagina web"
html_title:           "C++: Scaricare una pagina web"
simple_title:         "Scaricare una pagina web"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Che cosa e perché? 

Scaricare una pagina web significa prelevare e salvare il codice HTML di una determinata pagina internet nel tuo dispositivo. I programmatori fanno questo per analizzare il contenuto del codice, oppure per accedere più rapidamente ai dati dalla pagina nel futuro.

## Come fare:

Utilizzeremo il metodo `HttpClient.GetAsync()` per scaricare la pagina web. Ecco un esempio di come potrebbe apparire:

```C#
using System;
using System.Net.Http;
using System.Threading.Tasks;

class Program
{
    private static readonly HttpClient client = new HttpClient();

    static async Task Main()
    {
        string url = "http://example.com";
        string content = await client.GetStringAsync(url);
        Console.WriteLine(content);
    }
}
```

Questo codice stampa il contenuto HTML della pagina "http://example.com" sulla console.

## Approfondimento

Intorno al 2000, i programmatori usavano il metodo `WebClient.DownloadString`. Tuttavia, a partire dal .Net 4.5, si consiglia di usare `HttpClient.GetAsync` perché supporta l'asincronicità.

Un’alternativa potrebbe essere l’utilizzo della libreria `WebClient` invece di `HttpClient`, ma quest'ultimo è più flessibile e potente.

Per quanto riguarda i dettagli implementativi, `HttpClient.GetAsync` invia una richiesta GET all'URL specificato e restituisce una risposta HTTP, mentre `HttpClient.GetStringAsync(url)` restituisce il corpo della risposta come stringa.

## Vedi anche

- Documentazione ufficiale Microsoft sulla HttpClient Class: [link qui](https://docs.microsoft.com/it-it/dotnet/api/system.net.http.httpclient?view=net-5.0)

- Articolo su come scaricare file da internet in C#: [link qui](https://www.c-sharpcorner.com/article/how-to-download-a-file-from-internet-url-in-c/)

- Codice di esempio per HttpClient su GitHub per raccogliere informazioni web: [link qui](https://github.com/dotnet/samples/tree/main/snippets/csharp/VS_Snippets_Misc/hc_gettingstarted/cs)