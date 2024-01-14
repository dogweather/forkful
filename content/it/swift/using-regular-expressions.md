---
title:    "Swift: L'utilizzo delle espressioni regolari"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Perché

Le espressioni regolari sono uno strumento incredibilmente utile per il programmatore Swift. Con loro, è possibile effettuare ricerche e manipolazioni di testo in modo efficiente e preciso. Sebbene possano sembrare complesse all'inizio, una volta che si impara a usarle correttamente, possono risparmiare una quantità significativa di tempo e sforzo nella scrittura del codice.

## Come fare

Per utilizzare le espressioni regolari in Swift, è necessario importare il framework `Foundation`. Successivamente, è possibile creare un `NSRegularExpression` utilizzando il pattern desiderato, ad esempio:

```Swift
let regex = try NSRegularExpression(pattern: "[0-9]+")
```

Una volta creato, è possibile utilizzare il `NSRegularExpression` per cercare, sostituire o estrarre sezioni di testo in una stringa. Ad esempio, per trovare tutte le occorrenze di numeri in una stringa, si potrebbe utilizzare il metodo `matches(in:options:range:)` come segue:

```Swift
let matches = regex.matches(in: "Ho 5 mele e 3 banane", options: [], range: NSRange(location: 0, length: 20))

for match in matches {
  let range = match.range
  let string = (input as NSString).substring(with: range)
  print("Ho trovato il numero \(string)!")
}
```

Questo codice produrrebbe l'output:

```
Ho trovato il numero 5!
Ho trovato il numero 3!
```

Ci sono molte altre opzioni e metodi disponibili per le espressioni regolari in Swift, quindi assicurati di consultare la documentazione ufficiale per ulteriori dettagli.

## Approfondimento

Sebbene la sintassi delle espressioni regolari possa sembrare intimidatoria, vale la pena approfondire la loro conoscenza poiché possono essere estremamente utili in molte situazioni. Alcune cose da tenere a mente quando si lavora con espressioni regolari in Swift includono il fatto che sono case-sensitive e che è possibile utilizzare caratteri di escape per gestire caratteri speciali.

Inoltre, esistono diversi siti e tool online che possono aiutare nella creazione e nella verifica di espressioni regolari, come [RegExr](https://regexr.com/) e [Regex101](https://regex101.com/). Utilizzali per affinare le tue abilità e per testare le tue espressioni regolari prima di utilizzarle nel tuo codice Swift.

## Vedi anche

- [Documentazione ufficiale di Apple su `NSRegularExpression`](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [Tutorial sull'utilizzo di espressioni regolari in Swift](https://www.raywenderlich.com/5484335-regular-expressions-tutorial-getting-started)
- [Esempi pratici di utilizzo di espressioni regolari in Swift](https://www.swiftbysundell.com/articles/a-deep-dive-into-regular-expressions-in-swift/)