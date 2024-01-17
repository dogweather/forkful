---
title:                "Inviare una richiesta http"
html_title:           "C#: Inviare una richiesta http"
simple_title:         "Inviare una richiesta http"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/sending-an-http-request.md"
---

{{< edit_this_page >}}

Cosa è e perché inviare una richiesta HTTP?

In poche parole, inviare una richiesta HTTP significa comunicare con un server web per ottenere o inviare informazioni. I programmatori lo fanno per ottenere dati necessari per il loro software, come informazioni di login o contenuti di un sito web.

Come fare:

```C#
// Esempio di invio di una richiesta GET usando HttpClient
using System;
using System.Net.Http;

public static async Task Main()
{
    HttpClient client = new HttpClient();
    HttpResponseMessage response = await client.GetAsync("https://www.example.com");
    response.EnsureSuccessStatusCode();
    string responseBody = await response.Content.ReadAsStringAsync();
    Console.WriteLine($"Contenuto della pagina web: {responseBody}");
}
```

Output:

```
Contenuto della pagina web: <html>
<head>
    <title>Esempio</title>
</head>
<body>
    <h1>Benvenuto!</h1>
</body>
</html>
```

Deep Dive:

In passato, le richieste HTTP venivano gestite principalmente tramite le API wininet e winhttp di Microsoft. Tuttavia, con l'avanzamento della tecnologia e l'aumento della complessità delle applicazioni web, è diventato più conveniente utilizzare una libreria HTTP come HttpClient nel framework .NET Standard.

Alternative a HttpClient includono RestSharp e altre librerie di terze parti. Tuttavia, HttpClient rimane la scelta più popolare per gli sviluppatori C# grazie alla sua semplicità e facilità d'uso.

Per inviare una richiesta personalizzata, è possibile utilizzare l'oggetto HttpRequestMessage insieme all'oggetto HttpClient per aggiungere intestazioni, corpo e altro ancora alla richiesta.

Vedi anche:

- [Documentazione ufficiale di Microsoft su HttpClient](https://docs.microsoft.com/it-it/dotnet/api/system.net.http.httpclient?view=net-5.0)
- [Articolo su come utilizzare HttpClient in C#](https://www.c-sharpcorner.com/article/make-http-request-using-httpclient-in-C-Sharp/)
- [Altro esempio di HttpClient su GitHub](https://github.com/dotnet/runtime/blob/master/src/libraries/System.Net.Http/src/System/Net/Http/HttpClient.cs)