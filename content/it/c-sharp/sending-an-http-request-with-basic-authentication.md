---
title:                "Inviare una richiesta http con autenticazione di base."
html_title:           "C#: Inviare una richiesta http con autenticazione di base."
simple_title:         "Inviare una richiesta http con autenticazione di base."
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

L'invio di una richiesta HTTP con autenticazione di base è un'azione comune nella programmazione web. Consiste nell'inserimento di un codice o una password di base nell'intestazione HTTP della richiesta per accedere a risorse protette da credenziali.

I programmatori utilizzano l'autenticazione di base per garantire la sicurezza dei dati e delle risorse sensibili, limitando l'accesso solo agli utenti autorizzati.

## Come fare:

```C#
// Creazione di una nuova istanza della richiesta HTTP
var request = (HttpWebRequest)WebRequest.Create("url");

// Inserimento delle credenziali nell'intestazione della richiesta
request.Headers["Authorization"] = "Basic " + Convert.ToBase64String(Encoding.Default.GetBytes("username:password"));

// Invio della richiesta
var response = (HttpWebResponse)request.GetResponse();

// Ottenimento e lettura della risposta
var responseString = new StreamReader(response.GetResponseStream()).ReadToEnd();

// Output della risposta
Console.WriteLine(responseString);
```

## Approfondimento:

1. Contesto storico: L'autenticazione di base è uno dei primi metodi di autenticazione utilizzati in HTTP, introdotto nel protocollo nel 1999.
2. Alternative: Esistono diversi metodi di autenticazione più sicuri rispetto a quello di base, come ad esempio l'autenticazione OAuth o l'autenticazione a due fattori.
3. Dettagli implementativi: Le credenziali inserite nell'intestazione della richiesta vengono codificate in base64 per garantirne la sicurezza durante la trasmissione.

## Vedi anche:

- [Autenticazione di base in HTTP](https://tools.ietf.org/html/rfc7617)
- [Autenticazione in C#](https://docs.microsoft.com/en-us/dotnet/api/system.net.httpwebrequest.headers?view=netcore-3.1)