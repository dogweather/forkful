---
title:                "Swift: Utilizzare le espressioni regolari"
simple_title:         "Utilizzare le espressioni regolari"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Perché

Utilizzare le espressioni regolari può sembrare intimidatorio per molti programmatori, ma in realtà possono essere uno strumento molto utile per manipolare e verificare stringhe di testo. Sono particolarmente utili per trovare pattern all'interno di grandi quantità di dati, come ad esempio il parsing di file di log.

# Come utilizzare le espressioni regolari in Swift

Le espressioni regolari sono supportate nativamente in Swift tramite la libreria `Foundation`. Possiamo utilizzare il metodo `range(of: options:)` della classe `NSString` per cercare un pattern all'interno di una stringa. Ad esempio, se vogliamo cercare la parola "ciao" all'interno di una stringa, possiamo utilizzare il seguente codice:

```
let input = "Benvenuto nel mio blog"
let pattern = "ciao"
let range = (input as NSString).range(of: pattern)
```

Il metodo `range(of: options:)` restituirà un `NSRange` che rappresenta la posizione in cui è stata trovata la parola "ciao". Possiamo inoltre specificare delle opzioni per il modo in cui la ricerca deve essere effettuata, come ad esempio l'utilizzo di lettere accentate o l'ignorare la differenza tra lettere maiuscole e minuscole.

# Approfondimento su Expressions Regolari in Swift

Per una guida più completa su come utilizzare le espressioni regolari in Swift, è consigliato consultare la documentazione ufficiale di Apple sulla classe `NSRegularExpression` e la sua utilizzo. Inoltre, esistono diverse librerie di terze parti che offrono funzioni più avanzate per lavorare con le espressioni regolari in Swift, come ad esempio `Regex`, `SwiftRegex` e `SwiftPatterns`.

# Vedi anche

- [Documentazione ufficiale di Apple su NSRegularExpression](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [Libreria di terze parti Regex](https://github.com/sharplet/Regex)
- [Libreria di terze parti SwiftPatterns](https://github.com/geekcompany/SwiftPatterns) 
- [Libreria di terze parti SwiftRegex](https://github.com/andrewlord1990/SwiftRegex)