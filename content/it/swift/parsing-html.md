---
title:                "Analisi sintattica dell'html"
html_title:           "Bash: Analisi sintattica dell'html"
simple_title:         "Analisi sintattica dell'html"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/parsing-html.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?

Il parsing HTML significa analizzare un documento HTML per estrarre dati o manipolarne la struttura. Lo fanno i programmatori per accedere direttamente ai dati nel documento, per pulire o trasformare HTML, o per analizzare le pagine web.

## Come fare:

Ora impareremo a fare il parsing HTML in Swift usando il pacchetto SwiftSoup. Ecco un esempio di codice:

```Swift
import SwiftSoup

let html = "<html><head><title>Ciao, mondo!</title></head></html>"
do {
    let doc: Document = try SwiftSoup.parse(html)
    let titolo: Element = try doc.select("title").first()!
    print(titolo.text())
} catch Exception.Error(let type, let message) {
    print(message)
} catch {
    print("Errore sconosciuto")
}
```

Nell'output vedrai:

```
Ciao, mondo!
```

## Approfondimento

Il parsing HTML ha una lunga storia e vi sono molte alternative su come farlo. Nei primi giorni del web, molti programmatori eseguivano direttamente il parsing HTML. Con l'avvento di JavaScript e jQuery, è diventato più comune usare il DOM per ottenere accesso e manipolare HTML.

SwiftSoup è un'ottima scelta per fare parsing HTML in Swift, perché è completamente Swift e supporta molte delle funzionalità di JSoup, un popolare pacchetto Java per il parsing HTML. Tuttavia, ci sono anche altre opzioni come le API del WebKit o Kanna.

Le implementazioni dei parser HTML possono variare ampiamente. Alcuni lavorano direttamente con stringhe e utilizzano espressioni regolari, mentre altri, come SwiftSoup, costruiscono un DOM intero dell'HTML che quindi può essere manipolato.

## Vedi anche

Per ulteriori informazioni sulle possibilità con SwiftSoup e il parsing HTML, consulta le seguenti fonti:

- Documentazione SwiftSoup: [https://github.com/scinfu/SwiftSoup](https://github.com/scinfu/SwiftSoup)
- Tutorial di raywenderlich.com su SwiftSoup: [https://www.raywenderlich.com/347-swiftsoup-tutorial-getting-started](https://www.raywenderlich.com/347-swiftsoup-tutorial-getting-started)

Per capire meglio altre alternative per fare parsing HTML in Swift, dai un'occhiata a questi link:

- Documentazione Kanna: [https://github.com/tid-kijyun/Kanna](https://github.com/tid-kijyun/Kanna)
- WebKit DOM Parsing API: [https://developer.apple.com/documentation/webkit](https://developer.apple.com/documentation/webkit)