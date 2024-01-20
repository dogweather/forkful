---
title:                "Inviare una richiesta http"
html_title:           "C++: Inviare una richiesta http"
simple_title:         "Inviare una richiesta http"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Che Cosa & Perché?

Un HTTP request (richiesta HTTP) è un modo per i programmi di comunicare con i server o servizi web. Programatori lo sfruttano per richiedere, inviare o ricevere dati tramite internet.

## Come Fare:

In C#, la libreria `HttpClient` facilita l'invio di richieste HTTP. Ecco come farlo.

```C#
using System;
using System.Net.Http;

class Program
{
    static readonly HttpClient client = new HttpClient();

    static async Task Main()
    {
        HttpResponseMessage response = await client.GetAsync("https://esempio.com");
        response.EnsureSuccessStatusCode();
        string responseBody = await response.Content.ReadAsStringAsync();

        Console.WriteLine(responseBody);
    }
}
```

Questo programma invia una richiesta GET a 'https://esempio.com' e la risposta viene stampata sulla console.

## Approfondimento

1. **Nuances storiche**: HttpClient è stato introdotto in .NET Framework 4.5 per fornire un'alternativa più moderna alle classi WebRequest e WebResponse.

2. **Alternative**: Se stai lavorando con versioni più vecchie di .NET Framework, puoi utilizzare `WebRequest` e `WebResponse` per inviare richieste HTTP.

```C#
WebRequest request = WebRequest.Create("https://esempio.com");
WebResponse response = request.GetResponse();
```

3. **Dettagli di implementazione**: `HttpClient` gestisce automaticamente molte complessità come la gestione delle connessioni e assicura che le risorse di sistema vengano rilasciate correttamente. È consigliabile utilizzare un'istanza di HttpClient per più richieste invece di creare una nuova istanza per ciascuna richiesta.

## Vedi Anche:

1. [Documentazione ufficiale HttpClient](https://docs.microsoft.com/dotnet/api/system.net.http.httpclient)
2. [WebRequest vs HttpClient](https://stackoverflow.com/questions/37796954/webrequest-vs-httpclient)
3. [HttpClient Best Practices](https://aspnetmonsters.com/2016/08/2016-08-27-httpclientwrong/)
4. [HTTP Protocol Basics](https://www.ntu.edu.sg/home/ehchua/programming/webprogramming/HTTP_Basics.html)