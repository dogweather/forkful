---
title:                "C#: Scaricare una pagina web"
simple_title:         "Scaricare una pagina web"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

##Perché

Ci sono molti motivi per cui qualcuno potrebbe voler scaricare una pagina web. Potrebbe essere per fare uno snapshot di una pagina in un determinato momento o per analizzare il contenuto di una pagina web per ottenere informazioni utili. In ogni caso, il download di una pagina web è un'operazione importante per molti programmatori e può essere facilmente fatto utilizzando il linguaggio di programmazione C#.

##Come

Per eseguire il download di una pagina web in C#, dobbiamo utilizzare la classe WebClient. Prima di tutto, dobbiamo assicurarci di aver importato lo spazio dei nomi System.Net. Poi creeremo un'istanza di WebClient e utilizzeremo il metodo DownloadFile per scaricare la pagina web desiderata. Ecco un esempio di codice che mostra come scaricare la pagina di Google:

```C#
using System;
using System.Net;

namespace WebPageDownloader
{
    class Program
    {
        static void Main(string[] args)
        {
            // Creiamo un'istanza di WebClient
            WebClient webClient = new WebClient();
            // Impostiamo il percorso in cui salvare il file scaricato
            string filePath = @"C:\Users\NomeUtente\Desktop\google.html";
            // Utilizziamo il metodo DownloadFile per scaricare la pagina web desiderata
            webClient.DownloadFile("https://www.google.com", filePath);

            // Stampa un messaggio di successo
            Console.WriteLine("Pagina web scaricata con successo e salvata in: " + filePath);
        }
    }
}
```

Questo codice scaricherà la pagina di Google e la salverà nella posizione specificata dal percorso del file. Se apriamo il file salvato, vedremo il codice HTML della pagina di Google.

##Deep Dive

Se vogliamo esaminare più da vicino il contenuto della pagina web che abbiamo scaricato, possiamo utilizzare il metodo DownloadString invece di DownloadFile. Questo metodo restituirà il contenuto della pagina web come una stringa invece di scaricarlo come un file. Possiamo quindi utilizzare questa stringa per analizzare il codice HTML e recuperare le informazioni di cui abbiamo bisogno.

Oltre alla classe WebClient, esistono anche altre opzioni per scaricare una pagina web in C#, come ad esempio utilizzare la classe HttpClient o utilizzare una libreria esterna come HtmlAgilityPack. Ogni opzione ha i propri vantaggi e svantaggi, quindi è importante scegliere quella più adatta alle nostre esigenze.

##Vedi anche

- [Documentazione ufficiale di C# sulla classe WebClient](https://docs.microsoft.com/it-it/dotnet/api/system.net.webclient?view=netcore-3.1)
- [Esempio di utilizzo di HttpClient per scaricare una pagina web in C#](https://docs.microsoft.com/it-it/dotnet/csharp/programming-with-async-await#http-client-example)
- [Sito ufficiale di HtmlAgilityPack](https://html-agility-pack.net/)