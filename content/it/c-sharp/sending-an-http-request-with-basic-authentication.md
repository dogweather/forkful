---
title:                "Inviare una richiesta http con autenticazione di base"
html_title:           "Arduino: Inviare una richiesta http con autenticazione di base"
simple_title:         "Inviare una richiesta http con autenticazione di base"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Cos’è e Perché?

L’invio di una richiesta HTTP con l'autenticazione di base è un metodo per fornire credenziali username e password in una richiesta web. Lo facciamo per accedere a risorse protette.

## Come fare:

Ecco un esempio su come inviare una richiesta HTTP con l'autenticazione di base in C#:

```C#
using System;
using System.Net.Http;
using System.Text;

class Program
{
    static void Main()
    {
        string username = "your-username";
        string password = "your-password";

        HttpClient client = new HttpClient();

        var byteArray = Encoding.ASCII.GetBytes($"{username}:{password}");
        client.DefaultRequestHeaders.Authorization = new System.Net.Http.Headers.AuthenticationHeaderValue("Basic", Convert.ToBase64String(byteArray));

        HttpResponseMessage response = client.GetAsync("https://sito-seguro.com").Result;
        
        Console.WriteLine(response.StatusCode);
    }
}
```

Output di esempio:

``` 
HTTPStatusCode.OK
```

## Approfondimento:

L'autenticazione HTTP Basic è un sistema di autenticazione standardizzato molto antico in uso fin dal 1996, introdotto con la specifica HTTP 1.0. Nonostante ci siano alternative più sicure come l’OAuth e la JWT, la semplicità dell’HTTP Basic la rende ancora comunemente utilizzata.

Ricorda, le credenziali sono inviate come stringhe codificate in base64 non cifrate, quindi è vitale utilizzare HTTPS per garantire la sicurezza di tali informazioni durante il trasferimento.

## Vedi Anche:

- [Autenticazione HTTP su MDN](https://developer.mozilla.org/it/docs/Web/HTTP/Authentication)
- [HttpClient Class in .NET](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient)
- [AuthenticationHeaderValue Class in .NET](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.headers.authenticationheadervalue)