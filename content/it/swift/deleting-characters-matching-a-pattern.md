---
title:                "Swift: Eliminazione dei caratteri corrispondenti a un modello"
simple_title:         "Eliminazione dei caratteri corrispondenti a un modello"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Perché

Spesso ci troviamo a dover manipolare del testo all'interno dei nostri progetti di programmazione, e una delle operazioni più comuni è la rimozione di caratteri che rispettano un certo pattern. Questo può essere utile in molte situazioni, come ad esempio la pulizia di stringhe di input o la formattazione di dati.

## Come Fare

Per eliminare i caratteri che corrispondono a un certo pattern in Swift, possiamo utilizzare la funzione `replacingOccurrences(of:with:)` passando come primo parametro il pattern da cercare e come secondo parametro una stringa vuota. Ad esempio:

```Swift
let stringa = "Ciao! Questa è una stringa con alcuni caratteri speciali: %^&*"

let nuovaStringa = stringa.replacingOccurrences(of: "[^a-zA-Z0-9]", with: "", options: .regularExpression)

print(nuovaStringa)
// Output: CiaoQuestaunastringaconalcunicaratterispeciali
```

In questo caso, abbiamo utilizzato la sintassi regolare `[a-zA-Z0-9]` per specificare che vogliamo eliminare tutti i caratteri diversi da lettere maiuscole e minuscole e numeri.

Altre funzioni utili per la manipolazione di stringhe in Swift sono `trimmingCharacters(in:)` per eliminare spazi bianchi e newline all'inizio e alla fine di una stringa, e `components(separatedBy:)` per suddividere una stringa in base a un certo delimitatore.

## Approfondimento

Esistono diverse implementazioni possibili per la funzione `replacingOccurrences(of:with:)`, tra cui la creazione di un'espressione regolare personalizzata con `NSRegularExpression` oppure l'utilizzo della libreria `StringScanner`. Inoltre, è possibile utilizzare il parametro `options` per specificare opzioni aggiuntive per la ricerca, come ad esempio la ricerca case-insensitive o multilinea.

## Vedi Anche

- [Documentazione di Apple sulla manipolazione di stringhe in Swift](https://developer.apple.com/documentation/foundation/string)
- [Tutorial su come utilizzare espressioni regolari in Swift](https://www.raywenderlich.com/12160374-regular-expressions-in-swift-a-tutorial-for-programmers)