---
title:                "Analisi sintattica dell'HTML"
html_title:           "C++: Analisi sintattica dell'HTML"
simple_title:         "Analisi sintattica dell'HTML"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/parsing-html.md"
---

{{< edit_this_page >}}

## Che Cos'è e Perché?

L'analisi HTML è la pratica di interpretare una pagina web e suddividerla nei suoi elementi fondamentali. I programmatori la eseguono per estrarre dati specifici, manipolare gli elementi HTML o monitorare le modifiche al contenuto della pagina web.

## Come Fare:

Iniziamo con un esempio di base utilizzando la libreria Jsoup. Assicurati che il tuo progetto includa questa dipendenza per poterla usare.

```Java
//Includi la dipendenza Jsoup
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;

public class HtmlParsing {
    public static void main(String[] args) {
        String html = "<p>Un semplice <a href='https://www.google.com'>link</a>.</p>";
        Document doc = Jsoup.parse(html);
        Element link = doc.select("a").first();
        
        String text = doc.body().text(); // "Un semplice link."
        String linkHref = link.attr("href"); // "https://www.google.com"
        
        System.out.println(text);
        System.out.println(linkHref);
    }
}
```

Ecco l'output del nostro esempio:

```
Un semplice link.
https://www.google.com
```

## Approfondimento:

(1) L'analisi HTML è stata implementata fin dai primi giorni del web per consentire ai browser di visualizzare correttamente le pagine web. (2) Se Jsoup non è l'ideale per il tuo progetto, ci sono molte alternative come HtmlCleaner e Jericho HTML Parser. (3) L'implementazione dell'analisi HTML può diventare molto complessa quando si tratta di pagine web pesantemente annidate o mal formattate.

## Guarda Anche:

- Documentazione ufficiale Jsoup: [https://jsoup.org/](https://jsoup.org/)
- Alternativa HtmlCleaner: [https://htmlcleaner.sourceforge.io/](https://htmlcleaner.sourceforge.io/)
- Alternativa Jericho HTML Parser: [http://jericho.htmlparser.net/docs/index.html](http://jericho.htmlparser.net/docs/index.html)