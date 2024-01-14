---
title:    "Swift: Eliminazione di caratteri corrispondenti a un modello"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Perché

Eliminare i caratteri corrispondenti a un determinato modello può essere utile quando si vuole filtrare o modificare una stringa, ad esempio per rimuovere spazi vuoti o simboli di punteggiatura.

## Come fare

È possibile utilizzare il metodo `replacingOccurrences(of:with:)` sulla stringa di cui si desidera eliminare i caratteri. Ad esempio, se si vuole rimuovere tutti gli spazi vuoti dalla stringa `hello world`, si può scrivere:

```Swift
let stringa = "hello world"
let nuovaStringa = stringa.replacingOccurrences(of: " ", with: "")
print(nuovaStringa)
```

Output:
`helloworld`

## Approfondimento

Esistono diversi metodi per eliminare caratteri in Swift, come utilizzare l'operatore `!`, utilizzare la funzione globale `replace(_:with:options:range:)` o utilizzare espressioni regolari con il framework `Foundation`.

Un'altro modo per eliminare caratteri da una stringa è utilizzare il metodo `filter()` sulle sue caratteristiche:

```Swift
let stringa = "This is a sentence."
let caratteriDaRimuovere = "aeiou"
let nuovaStringa = stringa.filter { !caratteriDaRimuovere.contains($0) }
print(nuovaStringa)
```

Output:
`Ths s sntnc.`

# Vedi anche

- [metodo `replacingOccurrences(of:with:)`](https://developer.apple.com/documentation/foundation/nsstring/1417296-replacingoccurrences)
- [funzione globale `replace(_:with:options:range:)`](https://developer.apple.com/documentation/swift/string/1641127-replace)
- [framework `Foundation`](https://developer.apple.com/documentation/foundation)
- [metodo `filter()`](https://developer.apple.com/documentation/swift/array/2945498-filter)