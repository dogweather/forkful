---
title:                "Analisi dell'HTML"
date:                  2024-01-20T15:34:07.510905-07:00
html_title:           "Bash: Analisi dell'HTML"
simple_title:         "Analisi dell'HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/parsing-html.md"
---

{{< edit_this_page >}}

## Che Cosa & Perché?
L'interpretazione dell'HTML (parsing) serve a estrarre dati da pagine web. I programmatori lo fanno per analizzare, manipolare o estrarre informazioni in modo programmatico.

## Come Fare:
Swift non ha librerie standard per il parsing HTML, quindi usiamo `SwiftSoup`, una libreria di terze parti:

```swift
import SwiftSoup

let html = "<html><head><title>Ciao mondo!</title></head><body><p>Benvenuti nell'HTML parsing con Swift.</p></body></html>"

do {
    let doc = try SwiftSoup.parse(html)
    if let body = try doc.body() {
        let text = try body.text()
        print(text)  // Output: Benvenuti nell'HTML parsing con Swift.
    }
} catch Exception.Error(let type, let message) {
    print("Message: \(message)")
} catch {
    print("error")
}
```

Per installare `SwiftSoup`, usa Swift Package Manager o CocoaPods.

## Approfondimento:
Il parsing HTML in Swift è relativamente recente rispetto ad altri linguaggi come Python. `SwiftSoup` assomiglia a `jsoup` per Java, offrendo una manipolazione dell'HTML con una API di stile "jQuery". Alternative includono le libreria `Kanna` o l'uso di `WKWebView` per iniezione di JavaScript in una pagina web ed estrazione dei dati.

Gli aspetti tecnici comprendono l'handling degli errori nel parsing di HTML malformato e l'uso di selezionatori CSS per navigare il DOM (Document Object Model). 

La scelta della libreria può anche essere influenzata dalle prestazioni, dalla dimensione della libreria e dalla sua compatibilità con diversi ambienti Swift (iOS, macOS, server-side, ecc.).

## Vedi Anche:
- [SwiftSoup GitHub](https://github.com/scinfu/SwiftSoup)
- [Kanna GitHub](https://github.com/tid-kijyun/Kanna)
- [jsoup: Java HTML Parser](https://jsoup.org/)
- [Swift Package Manager](https://swift.org/package-manager/)
