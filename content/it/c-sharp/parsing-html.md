---
title:                "Analisi sintattica dell'HTML"
html_title:           "C++: Analisi sintattica dell'HTML"
simple_title:         "Analisi sintattica dell'HTML"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/parsing-html.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

"Parsing HTML" si riferisce alla decodifica di un documento HTML, estraendo dati utili da esso. I programmatori lo fanno per estrarre informazioni, manipolare il contenuto, o effettuare l'automazione sul web.

## Come fare:

Ecco un esempio di come fare il parsing HTML utilizzando la libreria `HtmlAgilityPack` in C#.

```C#
using HtmlAgilityPack;

public void EstraiDati()
{
    var doc = new HtmlDocument();
    doc.LoadHtml("<html><body><h1>Ciao, Mondo!</h1></body></html>");

    var nodo = doc.DocumentNode.SelectSingleNode("//h1");

    Console.WriteLine(nodo.InnerHtml);
}
```
Se eseguito, l'output sarà:

```
Ciao, Mondo!
```

## Approfondimento:

Historicamente, l'HTML non è sempre stato così strutturato come lo conosciamo oggi. Dunque, fare il parsing dell'HTML non è sempre stato così semplice. Oggi, la maggior parte dei documenti HTML seguono lo standard HTML5, rendendo il parsing molto più affidabile.

Ci sono diverse alternative per fare il parsing HTML. Alcune di queste includono l'uso di espressioni regolari (anche se non è consigliato per l'HTML), BeautifulSoup (per Python), JSoup (per Java), etc.

I dettagli dell'implementazione possono variare a seconda delle necessità del tuo progetto. Potresti aver bisogno di eseguire il parsing di un documento HTML completo o di cercare solo specifici tag HTML. Usare l'estrazione dei dati sul web dovrebbe essere fatto responsabilmente, rispettando le politiche del sito web e le leggi sulla privacy.

## Vedi anche:

- HtmlAgilityPack: [https://html-agility-pack.net/](https://html-agility-pack.net/)
- BeautifulSoup: [https://www.crummy.com/software/BeautifulSoup/](https://www.crummy.com/software/BeautifulSoup/)
- JSoup: [https://jsoup.org/](https://jsoup.org/) 
- Regole sullo scraping di dati: [https://www.imperva.com/learn/application-security/web-scraping/](https://www.imperva.com/learn/application-security/web-scraping/)