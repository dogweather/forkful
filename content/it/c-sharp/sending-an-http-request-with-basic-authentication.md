---
title:                "C#: Inviare una richiesta http con autenticazione di base"
simple_title:         "Inviare una richiesta http con autenticazione di base"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Perché

In questo blog post, scopriremo come inviare una richiesta HTTP con autenticazione di base utilizzando il linguaggio di programmazione C#. Questo può essere utile in situazioni in cui si desidera accedere a risorse protette da un server. 

## Come fare 

Il primo passo per inviare una richiesta HTTP con autenticazione di base è quello di importare il namespace `System.Net` nella nostra applicazione C#:

```C#
using System.Net;
```

Successivamente, dobbiamo creare un oggetto di tipo `HttpClient` e impostare le credenziali desiderate: 

```C#
var client = new HttpClient();

var username = "username";
var password = "password";

// Codifica le credenziali in base64
var authString = Convert.ToBase64String(Encoding.ASCII.GetBytes($"{username}:{password}"));

// Imposta l'intestazione di autenticazione della richiesta
client.DefaultRequestHeaders.Authorization = new AuthenticationHeaderValue("Basic", authString);
```

Infine, possiamo effettuare la nostra richiesta utilizzando il metodo `GetAsync` del nostro oggetto `HttpClient`:

```C#
var response = await client.GetAsync("https://www.example.com");

// Legge il contenuto della risposta
var responseContent = await response.Content.ReadAsStringAsync();
Console.WriteLine(responseContent);
```

L'esempio sopra crea una richiesta GET all'URL fornito utilizzando le credenziali specificate e stampa il contenuto della risposta sulla console.

## Approfondimento 

Il processo di autenticazione di base si basa sull'aggiunta di un'intestazione "Authorization" alla richiesta contenente le credenziali criptate in Base64. Questa forma di autenticazione non è sicura in quanto le credenziali vengono trasmesse in chiaro e possono quindi essere facilmente intercettate da un malintenzionato. Per aumentare la sicurezza, è possibile utilizzare l'autenticazione con chiavi di accesso (token) o utilizzare il protocollo HTTPS per crittografare la richiesta.

## Vedi anche

- [Documentazione di Microsoft su `HttpClient`](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient?view=netframework-4.8)
- [Tutorial su autenticazione HTTP con C#](https://www.red-gate.com/simple-talk/dotnet/net-development/youre-encrypting-passwords-wrong/)