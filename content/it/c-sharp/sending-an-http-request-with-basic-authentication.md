---
title:                "Invio di una richiesta http con autenticazione di base"
html_title:           "C#: Invio di una richiesta http con autenticazione di base"
simple_title:         "Invio di una richiesta http con autenticazione di base"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Perché

Invio di una richiesta HTTP con autenticazione di base è un passo essenziale per comunicare con una risorsa protetta su Internet, in particolare su servizi web o API. Questo tipo di autenticazione fornisce un livello di sicurezza aggiuntivo per accedere a dati e informazioni sensibili.

## Come fare

Per inviare una richiesta HTTP con autenticazione di base in C#, è necessario seguire i seguenti passaggi:

1. Creare un oggetto `HttpClient` per gestire la comunicazione con il server:

```
HttpClient client = new HttpClient();
```

2. Aggiungere le credenziali di autenticazione di base all'header della richiesta:

```
client.DefaultRequestHeaders.Authorization = new AuthenticationHeaderValue("Basic", 
                        Convert.ToBase64String(Encoding.ASCII.GetBytes("username:password")));
```

3. Creare e configurare l'oggetto `HttpRequestMessage` per specificare il metodo, l'URL e i parametri della richiesta:

```
HttpRequestMessage request = new HttpRequestMessage(HttpMethod.Get, "https://example.com/api/data");
request.Headers.Accept.Add(new MediaTypeWithQualityHeaderValue("application/json"));
```
    
4. Eseguire la richiesta utilizzando il metodo `SendAsync` della classe `HttpClient` e ottenere la risposta:

```
HttpResponseMessage response = await client.SendAsync(request);
```

5. Leggere il contenuto della risposta utilizzando il metodo `ReadAsStringAsync` e ottenere i dati desiderati:

```
string result = await response.Content.ReadAsStringAsync();
Console.WriteLine(result);
```

Ecco un esempio completo di come inviare una richiesta GET con autenticazione di base utilizzando C# e leggere la risposta:

```
using System;
using System.Net.Http;
using System.Net.Http.Headers;
using System.Text;

namespace BasicAuthExample
{
    class Program
    {
        static async System.Threading.Tasks.Task Main(string[] args)
        {
            HttpClient client = new HttpClient();

            client.DefaultRequestHeaders.Authorization = new AuthenticationHeaderValue("Basic", 
                        Convert.ToBase64String(Encoding.ASCII.GetBytes("username:password")));

            HttpRequestMessage request = new HttpRequestMessage(HttpMethod.Get, "https://example.com/api/data");
            request.Headers.Accept.Add(new MediaTypeWithQualityHeaderValue("application/json"));

            HttpResponseMessage response = await client.SendAsync(request);
            string result = await response.Content.ReadAsStringAsync();

            Console.WriteLine(result);
        }
    }
}
```

E questo sarebbe il risultato di esecuzione del codice:

```
{
    "id": 1234,
    "name": "John Doe",
    "email": "john.doe@example.com"
}
```

## Approfondimento

L'autenticazione di base è uno dei tanti modi di autenticazione disponibili per le richieste HTTP. In questo metodo, le credenziali dell'utente vengono codificate in Base64 e inviate insieme alla richiesta. Questo tipo di autenticazione, tuttavia, non è considerato sicuro poiché i dati sono facilmente decodificabili e possono essere vulnerabili ad attacchi di man-in-the-middle.

Tuttavia, l'autenticazione di base può essere utile per risorse con dati meno critici o in combinazione con altri metodi di autenticazione più sicuri.

## Vedi anche

* [Autenticazione di base - Wikipedia](https://it.wikipedia.org/wiki/Autenticazione_di_base)
* [Documentazione ufficiale di Microsoft per HttpClient Class](https://docs.microsoft.com/it-it/dotnet/api/system.net.http.httpclient?view=net-5.0)