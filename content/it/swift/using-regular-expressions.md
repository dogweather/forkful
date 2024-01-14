---
title:    "Swift: Utilizzando le espressioni regolari"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Perché

RegEx, abbreviazione di espressioni regolari, è uno strumento di programmazione potente per la ricerca e la manipolazione di testo. È utile per trovare corrispondenze di pattern in una stringa o per sostituire parti della stringa con un'altra. Se sei uno sviluppatore Swift, è importante avere familiarità con le espressioni regolari per semplificare e velocizzare le tue attività di programmazione.

## Come Fare

In Swift, puoi utilizzare le espressioni regolari utilizzando l'API NSRegularExpression. Segui questi semplici passi per utilizzarlo nel tuo codice:

1. Crea un'espressione regolare utilizzando il modello desiderato, ad esempio "123" per trovare tutti i numeri "123" in una stringa.
2. Usa la funzione `NSRegularExpression()` per creare una nuova istanza di espressione regolare, passando il modello e le opzioni desiderate.
3. Usa la funzione `matches(in:options:range:)` per ottenere un array di NSTextCheckingResult che contengono le corrispondenze trovate nella tua stringa.
4. Accedi all'NSTextCheckingResult restituito per ottenere i dettagli della corrispondenza, come la posizione e la lunghezza della corrispondenza nella stringa originale.

Di seguito è riportato un esempio pratico di come utilizzare le espressioni regolari per trovare tutte le parole con almeno 3 lettere in una stringa:

```Swift
let string = "Questo è un esempio di stringa con parole di varie lunghezze."
let pattern = "[a-zA-Z]{3,}" // modello che trova parole con almeno 3 lettere
let regex = try! NSRegularExpression(pattern: pattern, options: [])
let matches = regex.matches(in: string, options: [], range: NSRange(location: 0, length: string.utf16.count))
for match in matches {
    let wordRange = match.range
    if let range = Range(wordRange, in: string) {
        let word = string[range]
        print(word) // output: Questo, esempio, stringa, con, parole, varie, lunghezze
    }
}
```

## Approfondimento

Le espressioni regolari offrono una vasta gamma di funzionalità, come i gruppi di cattura per ricavare parti specifiche della corrispondenza e le opzioni per controllare la sensibilità alle maiuscole e minuscole. Ci sono anche molte risorse online, come il sito regex101.com, che possono aiutarti a costruire e testare i tuoi modelli di espressioni regolari.

Ricorda che le espressioni regolari possono essere abbastanza complesse e possono richiedere del tempo per padroneggiarle completamente, quindi non scoraggiarti se inizi a utilizzarle. Continua a praticare e scoprirai quanto possano essere potenti e utili nella tua programmazione.

## Vedi Anche

- [NSRegularExpression Apple Developer Documentation](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [Regex101](https://regex101.com/)
- [Regular Expressions in Swift: Part 1](https://www.raywenderlich.com/5766-regular-expressions-tutorial-getting-started)
- [Regular Expressions in Swift: Part 2](https://www.raywenderlich.com/5772-regular-expressions-tutorial-advanced)