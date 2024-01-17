---
title:                "Analisi di HTML"
html_title:           "Java: Analisi di HTML"
simple_title:         "Analisi di HTML"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/parsing-html.md"
---

{{< edit_this_page >}}

## Cosa e perché?
Il parsing di HTML è il processo attraverso il quale un programma legge e interpreta il linguaggio di marcatura utilizzato per creare pagine web. I programmatori lo fanno per estrarre informazioni specifiche dalle pagine web, come ad esempio il contenuto di un articolo o il prezzo di un prodotto.

## Come?
Di seguito viene mostrato un esempio di codice Java per eseguire il parsing di HTML utilizzando la libreria Jsoup. In questo esempio, si sta cercando di ottenere il contenuto di un elemento con un determinato ID all'interno di una pagina web.

```Java
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;

public class HtmlParser {

    public static void main(String[] args) {
        // Specifica l'URL della pagina web da analizzare
        String url = "https://www.esempio.com/pagina.html";

        try {
            // Utilizza Jsoup per connettersi alla pagina web e ottenere il suo contenuto
            Document doc = Jsoup.connect(url).get();

            // Utilizza il metodo getElementById per ottenere l'elemento desiderato
            Element elemento = doc.getElementById("idElemento");

            // Stampa il contenuto dell'elemento
            System.out.println("Contenuto dell'elemento: " + elemento.text());
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

Output:
`Contenuto dell'elemento: Questo è il testo dell'elemento desiderato.`

## Approfondimento
Il parsing di HTML è diventato necessario con lo sviluppo delle pagine web e il loro aumentare di complessità. In passato, i programmatori dovevano scrivere il loro parser personalizzato per ogni sito web che volevano analizzare. Oggi, è possibile utilizzare librerie come Jsoup o JsoupXpath per semplificare e automatizzare questo processo.

## Vedi anche
- [Jsoup](https://jsoup.org/)
- [JsoupXpath](https://jsoupxpath.com/)