---
title:    "Swift: Eliminazione di caratteri corrispondenti a uno schema"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Perché

Ci sono molte ragioni per cui potresti voler cancellare dei caratteri che corrispondono ad un determinato modello in Swift. Forse vuoi pulire i dati di input da caratteri speciali o vuoi rimuovere le emoticon da un testo. Qualunque sia il motivo, imparare come farlo può semplificare il tuo codice e rendere il tuo programma più efficiente.

## Come

Per cancellare i caratteri che corrispondono ad un modello in Swift, utilizzeremo la funzione `filter` che accetta una chiusura (closure) come parametro. La chiusura viene eseguita per ogni carattere nella stringa e determina se il carattere deve essere incluso o meno nel risultato finale.

Ecco un esempio di codice in Swift che rimuove tutte le lettere minuscole da una stringa:

```Swift
let input = "H3ll0 W0rld! 🌎"
let output = input.filter { character in
    return !character.isLowercase
}

print(output) // Output: H3! 🌎
```

Nell'esempio sopra, abbiamo usato la funzione `isLowercase` per determinare se il carattere è una lettera minuscola o meno. Se il carattere è una lettera minuscola, viene escluso dal risultato finale. Possiamo anche utilizzare altri metodi come `isUppercase` o `isNumber` per filtrare caratteri diversi.

## Deep Dive

Per coloro che vogliono approfondire, esiste un'alternativa alla funzione `filter` per cancellare i caratteri che corrispondono a un modello in Swift. Possiamo utilizzare la funzione `replacingOccurrences` che sostituisce una stringa con un'altra stringa specificata.

Ecco un esempio di codice che utilizza la funzione `replacingOccurrences` per rimuovere tutti i caratteri non numerici da una stringa:

```Swift
let input = "H3ll0 W0rld! 🌎"
let output = input.replacingOccurrences(of: "[^0-9]", with: "", options: .regularExpression)

print(output) // Output: 30
```

In questo caso, stiamo utilizzando l'espressione regolare `[^0-9]` che corrisponde a tutti i caratteri che non sono numerici. Utilizzando la funzione `replacingOccurrences`, sostituiamo questi caratteri con una stringa vuota, ottenendo così una stringa solo con numeri.

## Vedi anche

- [La documentazione ufficiale di Swift sulla funzione `filter`](https://developer.apple.com/documentation/swift/array/1688939-filter)
- [La documentazione ufficiale di Swift sulla funzione `replacingOccurrences`](https://developer.apple.com/documentation/foundation/nsstring/1415564-replacingoccurrences)
- [Un tutorial dettagliato sull'uso delle espressioni regolari in Swift](https://www.raywenderlich.com/576-ios-regexp)