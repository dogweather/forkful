---
title:                "Estrazione HTML"
html_title:           "Swift: Estrazione HTML"
simple_title:         "Estrazione HTML"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/parsing-html.md"
---

{{< edit_this_page >}}

## Perché

Se stai sviluppando un'applicazione iOS o macOS che deve interagire con il web, potresti trovarti nella situazione di dover analizzare il codice HTML delle pagine web per accedere ai dati di cui hai bisogno. In questo caso, la funzionalità di parsing HTML diventa essenziale per il tuo progetto.

## Come fare

Per prima cosa, assicurati di aver importato il framework Foundation nel tuo progetto per poter utilizzare le classi di supporto per il parsing HTML.

```Swift
import Foundation
```

Quindi, puoi utilizzare il delegato degli oggetti URLSession per ottenere i dati della pagina web in formato Data. Successivamente, utilizza l'oggetto HTMLParser per analizzare il codice HTML e ottenere il contenuto di cui hai bisogno.

```Swift
// Esempio di utilizzo del delegato URLSession
let session = URLSession.shared.dataTask(with: URL(string: "https://www.example.com")!) { (data, response, error) in 

    // Utilizzo dell'oggetto HTMLParser
    let parser = HTMLParser(data: data!)
    parser.parse { (document, error) in
        print(document?.body?.text ?? "")
    }
}
session.resume()
```

Dopo aver ottenuto il contenuto, puoi utilizzare le proprietà e i metodi dell'oggetto HTMLDocument per navigare e ottenere i dati di cui hai bisogno.

```Swift
// Esempio di utilizzo delle proprietà e dei metodi di HTMLDocument
if let title = document?.title {
    print("Titolo della pagina: \(title)")
}
if let links = document?.links {
    print("Lista dei link presenti nella pagina:")
    for link in links {
        print("- \(link)")
    }
}
```

## Approfondimento

Il parsing HTML può risultare complesso, soprattutto quando si tratta di pagine con una struttura più complessa o con codice non standard. In questi casi, potrebbe essere utile utilizzare librerie esterne come SwiftSoup o Kanna per semplificare il processo di parsing e ottenere i dati desiderati.

Tuttavia, è importante ricordare che il parsing HTML non è una soluzione ideale per ottenere i dati da una pagina web. Spesso, è preferibile utilizzare un API esposta dal sito stesso o un servizio di scraping professionale per evitare problemi di affidabilità e conformità alle normative di utilizzo dei dati.

## Vedi anche

Per ulteriori informazioni sul parsing HTML con Swift, puoi consultare i seguenti link:

- [Documentazione ufficiale di Apple su Foundation](https://developer.apple.com/documentation/foundation)
- [Libreria SwiftSoup](https://github.com/scinfu/SwiftSoup)
- [Libreria Kanna](https://github.com/tid-kijyun/Kanna)