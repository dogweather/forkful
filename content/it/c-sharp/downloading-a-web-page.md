---
title:                "Scaricare una pagina web."
html_title:           "C#: Scaricare una pagina web."
simple_title:         "Scaricare una pagina web."
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Perché
C'è una varietà di ragioni per cui potresti voler scaricare una pagina web. Potresti voler salvare una copia locale della pagina per accedervi in futuro anche senza una connessione internet, o potresti voler analizzare il contenuto della pagina per estrarre informazioni rilevanti.

## Come
```C#
// Utilizzando la libreria System.Net.Http per fare una richiesta HTTP
using System.Net.Http;

// Creazione di un oggetto HttpClient
HttpClient client = new HttpClient();

// Utilizzo del metodo GetAsync per ottenere i dati della pagina web
HttpResponseMessage response = await client.GetAsync("https://www.example.com");

// Leggere i dati come stringa
string pageContent = await response.Content.ReadAsStringAsync();

// Stampa dei dati ottenuti
Console.WriteLine(pageContent);
```

Output:
```
<!DOCTYPE html>
<html>
<head>
<title>Esempio</title>
</head>
<body>
<h1>Benvenuto</h1>
<p>Questo è un esempio di pagina web.</p>
</body>
</html>
```

## Deep Dive
Oltre a scaricare il contenuto della pagina, è possibile anche ottenere altre informazioni come l'header della risposta HTTP, i cookie e i parametri della richiesta. Inoltre, è possibile utilizzare librerie esterne, come HtmlAgilityPack, per analizzare il contenuto HTML della pagina in modo più strutturato.

## Vedi Anche
- [Documentazione di HttpClient su Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient?view=net-5.0)
- [HtmlAgilityPack su NuGet](https://www.nuget.org/packages/HtmlAgilityPack/)