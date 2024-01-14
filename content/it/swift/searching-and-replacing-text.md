---
title:    "Swift: Ricerca e sostituzione di testo"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore Swift, è molto probabile che tu abbia dovuto cercare e sostituire del testo in vari punti del tuo codice. Questo può essere dovuto a diversi motivi, come la correzione di un errore di battitura o la modifica di una variabile in molti punti del tuo progetto. In ogni caso, la funzione di ricerca e sostituzione è uno strumento essenziale per semplificare il processo di modifica del tuo codice.

## Come fare

Per cercare e sostituire del testo in Swift, puoi utilizzare la funzione `replacingOccurrences(of:with:)` dell'oggetto `String`. Questa funzione accetta due parametri: il testo che vuoi sostituire e il testo con cui vuoi sostituirlo. Di seguito è riportato un esempio di codice che utilizza questa funzione per sostituire la parola "ciao" con "salve" in una stringa:

```
let greeting = "Ciao mondo!"
let newGreeting = greeting.replacingOccurrences(of: "ciao", with: "salve")
print(newGreeting) // Salve mondo!
```

Nota che questa funzione sostituirà solo la prima occorrenza del testo specificato nella stringa. Se vuoi sostituire tutte le occorrenze, puoi utilizzare il metodo `replacingOccurrences(of:with:options:)` e impostare il parametro `options` su `.regularExpression` per utilizzare le espressioni regolari. Ad esempio:

```
let numbers = "1 2 3 4 5"
let updatedNumbers = numbers.replacingOccurrences(of: "\\d", with: "*", options: .regularExpression)
print(updatedNumbers) // * * * * *
```

In questo caso, tutti i numeri nella stringa vengono sostituiti con l'asterisco.

## Approfondimento

Oltre alla funzione di base `replacingOccurrences(of:with:)`, esistono altre funzioni che puoi utilizzare per cercare e sostituire del testo in Swift. Ad esempio, puoi utilizzare il metodo `replaceAll()` dell'oggetto `NSMutableString` per sostituire tutte le occorrenze di una stringa con un'altra stringa. Inoltre, puoi utilizzare il framework `NSRegularExpression` per eseguire sostituzioni basate su espressioni regolari.

Inoltre, è possibile utilizzare la funzione `String.replacingOccurrences(of:with:options:range:)` per specificare una determinata porzione della stringa in cui eseguire la sostituzione invece di sostituire in tutta la stringa.

## Vedi anche

- Documentazione di Apple su `String.replacingOccurrences(of:with:)`: https://developer.apple.com/documentation/swift/string/3126570-replacingoccurrences
- Tutorial su espressioni regolari in Swift: https://www.raywenderlich.com/86205/nsregularexpression-swift-tutorial
- Tutorial su come utilizzare `NSMutableString`: https://www.hackingwithswift.com/example-code/strings/how-to-replace-a-substring-with-another-substring-using-replace