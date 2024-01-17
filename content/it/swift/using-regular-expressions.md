---
title:                "Utilizzare le espressioni regolari"
html_title:           "Swift: Utilizzare le espressioni regolari"
simple_title:         "Utilizzare le espressioni regolari"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Che cos'è e perché usarlo?

Le espressioni regolari sono uno strumento utilizzato dai programmatori per gestire ricerce avanzate all'interno di testi. Ciò può semplificare e velocizzare l'analisi di stringhe di testo, consentendo di individuare pattern specifici o eseguire sostituzioni di testo in modo efficiente.

## Come utilizzarle:

Le espressioni regolari vengono utilizzate nel linguaggio Swift mediante una classe chiamata `NSRegularExpression`. Per utilizzarla, è necessario includere la seguente dichiarazione all'inizio del codice:

`import Foundation`

Dopo di che, possiamo creare un'istanza della classe `NSRegularExpression` come mostrato di seguito:

```
let regex = try NSRegularExpression(pattern: "[abc]", options: [])
```

In questo esempio, stiamo cercando un pattern che corrisponda a una delle lettere a, b o c.

Per eseguire una ricerca all'interno di una stringa, utilizziamo il metodo `matches(in:options:range:)` passando come parametro la stringa in cui eseguire la ricerca. Questo metodo restituisce un array di oggetti `NSTextCheckingResult` contenente tutte le corrispondenze trovate. Possiamo quindi utilizzare il metodo `range` per ottenere la posizione della corrispondenza all'interno della stringa originale.

```
let text = "abc123"
let matches = regex.matches(in: text, options: [], range: NSRange(location: 0, length: text.utf16.count))

for match in matches {
    let range = match.range
    // qui possiamo effettuare le operazioni desiderate con la posizione della corrispondenza trovata
}
```

## Approfondimento:

Le espressioni regolari sono state introdotte da Stephen Kleene negli anni '50 per rappresentare linguaggi formali come i linguaggi di programmazione. Oggi vengono utilizzate in molti linguaggi di programmazione, compreso Swift, per gestire ricerche avanzate all'interno di stringhe di testo.

Come alternativa alle espressioni regolari, è possibile utilizzare il framework `String` di Swift per effettuare ricerche di testo più semplici e generiche. Tuttavia, le espressioni regolari offrono una maggiore precisione e flessibilità per la ricerca di pattern specifici.

Per maggiori informazioni sull'utilizzo delle espressioni regolari in Swift, si consiglia la documentazione Apple [Regular Expressions](https://developer.apple.com/documentation/foundation/nsregularexpression).

## Altri riferimenti:

- [Regular Expression Cheat Sheet](https://www.rexegg.com/regex-quickstart.html)
- [Regex Tutorial](https://www.regular-expressions.info/tutorial.html)