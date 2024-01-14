---
title:                "Swift: Capitalizzare una stringa"
simple_title:         "Capitalizzare una stringa"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché

Capire come capitalizzare una stringa è essenziale per la gestione dei dati in Swift. Questa operazione può essere utile per uniformare l'aspetto di una stringa o per confrontare correttamente due stringhe.

## Come farlo

Utilizzando il metodo `uppercased()` possiamo ottenere una nuova stringa con tutti i caratteri convertiti in maiuscolo. Vediamo un esempio usando una variabile `name` e stampando l'output:

```Swift
let name = "maria"
print(name.uppercased())

// Output: MARIA
```

Possiamo anche utilizzare il metodo `capitalized()` per ottenere una nuova stringa con il primo carattere convertito in maiuscolo. Vediamo un altro esempio usando una variabile `city` e stampando l'output:

```Swift
let city = "roma"
print(city.capitalized())

// Output: Roma
```

È anche possibile capitalizzare una stringa specifica utilizzando il metodo `replacingOccurrences(of:with:)`:

```Swift
let phrase = "questa è una frase"
print(phrase.replacingOccurrences(of: "f", with: "F"))

// Output: questa è una frase
```

## Approfondimento

Il metodo `uppercased()` utilizza il locale predefinito del dispositivo per la conversione in maiuscolo dei caratteri. Tuttavia, è possibile specificare manualmente il locale passandolo come argomento al metodo. Ad esempio, per garantire che i caratteri dell'alfabeto italiano vengano capitalizzati correttamente, possiamo utilizzare il locale `it_IT`:

```Swift
let phrase = "questa è una frase"
print(phrase.uppercased(locale: Locale(identifier: "it_IT")))

// Output: QUESTA È UNA FRASE
```

È importante notare che il metodo `capitalized()` non solo converte il primo carattere della stringa in maiuscolo, ma anche tutti i caratteri successivi che sono preceduti da uno spazio, un segno di punteggiatura o un simbolo di punteggiatura.

## Vedi anche

- [Documentazione Apple su String](https://developer.apple.com/documentation/swift/string)
- [Tutorial su String in Swift](https://www.raywenderlich.com/511-swift-string-cheat-sheet)