---
title:                "Unendo stringhe"
html_title:           "Swift: Unendo stringhe"
simple_title:         "Unendo stringhe"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché

La concatenazione di stringhe è una tecnica importante da imparare per tutti gli sviluppatori Swift. Quando si lavora con testi e messaggi, spesso è necessario combinare diverse stringhe in una sola, e la concatenazione è il modo più semplice e veloce per farlo.

## Come fare

La concatenazione di stringhe in Swift è molto semplice! Utilizzando l'operatore `+`, è possibile unire due o più stringhe in una sola. Ecco un esempio di codice:

```Swift
let nome = "Maria"
let saluto = "Ciao"
let salutoCompleto = saluto + " " + nome
print(salutoCompleto) // Output: Ciao Maria
```

Come puoi vedere, è possibile unire non solo stringhe, ma anche stringhe con lettere ed altri caratteri.

## Approfondimento

In Swift, esistono diversi modi per concatenare le stringhe oltre all'uso dell'operatore `+`. Ad esempio, è possibile utilizzare il metodo `joined(separator:)` per unire una raccolta di stringhe in una sola, separandole con un separatore specifico. Ecco un esempio:

```Swift
let pasti = ["Colazione", "Pranzo", "Cena"]
let pastiCompleti = pasti.joined(separator: ", ")
print(pastiCompleti) // Output: Colazione, Pranzo, Cena
```

Inoltre, esiste anche il metodo `appending(_:)` che consente di aggiungere una stringa alla fine di un'altra stringa senza dover utilizzare l'operatore `+`, come in questo esempio:

```Swift
let saluto = "Ciao"
let salutoCompleto = saluto.appending(" amico!")
print(salutoCompleto) // Output: Ciao amico!
```

Ora che conosci diversi modi per concatenare le stringhe in Swift, puoi utilizzare quello che meglio si adatta alla tua situazione e al tuo stile di codifica.

## Vedi anche
- [Documentazione di Swift su stringhe](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Tutorial su concatenazione di stringhe in Swift](https://www.raywenderlich.com/751-swift-tutorial-string-array-dictionaries-loops-conditions)
- [Altro articolo su Swift per sviluppatori](https://www.raywenderlich.com/2163-swift-tutorial-part-1-expressions-variables-and-constants)