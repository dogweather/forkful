---
title:                "Scaricare una pagina web"
html_title:           "C#: Scaricare una pagina web"
simple_title:         "Scaricare una pagina web"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?
Scaricare una pagina web significa ottenere il suo contenuto attraverso il web. I programmatori spesso lo fanno per accedere a dati o informazioni contenute in una pagina. Per esempio, è possibile scaricare i dati di un sito di notizie per analizzarli estrarre informazioni per un progetto.

## Come fare:
Per scaricare una pagina web in C#, puoi utilizzare la classe ```WebClient``` dalla libreria standard di .NET. Basta avere l'URL della pagina come input e usare il metodo ```DownloadString```, che restituirà il contenuto della pagina come una stringa. Ad esempio:

```C#
using System.Net;

string url = "https://www.example.com";
using(WebClient client = new WebClient()){
  string pageContent = client.DownloadString(url);
  Console.WriteLine(pageContent);
}
```
Il risultato sarà il contenuto della pagina stampato nella console.

## Approfondimento:
Scaricare una pagina web è una pratica molto comune nello sviluppo software moderno. Prima dell'avvento delle applicazioni web, i programmatori dovevano scaricare i contenuti delle pagine per mostrarli nel browser. Oggi, è più spesso utilizzato per ottenere dati per l'analisi o l'integrazione in altri progetti. Ci sono anche diverse alternative per questo processo, come l'utilizzo di librerie di scraping o API specifiche.

## Vedi anche:
- Documentazione ufficiale di ```WebClient```: https://docs.microsoft.com/en-us/dotnet/api/system.net.webclient
- Una guida dettagliata sul processo di download di una pagina web in C#: https://www.c-sharpcorner.com/blogs/simple-tips-to-download-htmlweb-page-using-c-sharp-code-snippet
- Un tutorial su come scrivere uno scraper in C#: https://www.pluralsight.com/guides/building-a-simple-web-scraper-in-csharp-using-htmlagilitypack