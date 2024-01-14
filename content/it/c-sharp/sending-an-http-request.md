---
title:                "C#: Inviare una richiesta http"
simple_title:         "Inviare una richiesta http"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte ragioni per cui potresti voler inviare una richiesta HTTP dal tuo programma in C#. Ad esempio, potresti dover accedere a dati da un server remoto o inviare informazioni a un'applicazione web. In generale, l'invio di richieste HTTP è un'attività comune per molte applicazioni che comunicano attraverso la rete.

## Come Fare

Invio di una richiesta GET:

```C#
var url = "http://www.example.com"; // Cambia l'URL secondo le tue esigenze
            
// Crea una nuova istanza di HttpClient
using (var client = new HttpClient())
{
    // Invia una richiesta GET all'URL specificato
    HttpResponseMessage response = await client.GetAsync(url);
    
    // Leggi la risposta come una stringa
    string responseString = await response.Content.ReadAsStringAsync();
    
    // Stampa la risposta
    Console.WriteLine(responseString);
}
```

Output:
```
<html>
  <head>
    <title>Esempio</title>
  </head>
  <body>
    <h1>Ciao dal sito di esempio!</h1>
  </body>
</html>
```

Invio di una richiesta POST:

```C#
// Dati da inviare nel body della richiesta
var postData = new Dictionary<string, string>(){
    {"username","mario89"},
    {"password","segreto123"},
};

var url = "http://www.example.com/login";

// Crea una codifica dei dati come application/x-www-form-urlencoded
var formData = new FormUrlEncodedContent(postData);

// Crea una nuova istanza di HttpClient
using (var client = new HttpClient())
{
    // Invia una richiesta POST all'URL specificato con i dati del form
    HttpResponseMessage response = await client.PostAsync(url, formData);
    
    // Leggi la risposta come una stringa
    string responseString = await response.Content.ReadAsStringAsync();
    
    // Stampa la risposta
    Console.WriteLine(responseString);
}
```

Output:
```
<p>Benvenuto, mario89!</p>
```

## Approfondimento

L'invio di richieste HTTP può essere ulteriormente personalizzato utilizzando le diverse opzioni disponibili in HttpClient. Ad esempio, puoi specificare gli header della richiesta, impostare dei timeout e gestire i cookie. Inoltre, puoi utilizzare librerie come "Newtonsoft.Json" per gestire facilmente formati come JSON nelle tue richieste e risposte.

## Vedi Anche

- [HttpClient Class (System.Net.Http)](https://docs.microsoft.com/it-it/dotnet/api/system.net.http.httpclient?view=netcore-3.1)
- [Eseguire richieste HTTP con HttpClient in C#](https://docs.microsoft.com/it-it/dotnet/api/system.net.http.httpclient?view=netcore-3.1)
- [Cosa sono le richieste HTTP?](https://developer.mozilla.org/it/docs/Web/HTTP/Basics_of_HTTP/Identifying_resources_on_the_Web)