---
title:                "Swift: Trovare la lunghezza di una stringa"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Perché 

Trovare la lunghezza di una stringa è una delle operazioni di base che ogni programmatore deve essere in grado di fare. È utile per molte cose, come la validazione dei dati inseriti dagli utenti o la manipolazione di stringhe in una determinata funzione. In questo articolo, esploreremo come trovare la lunghezza di una stringa utilizzando il linguaggio di programmazione Swift.

## Come fare

Per trovare la lunghezza di una stringa in Swift, utilizzeremo il metodo `count`, che restituisce il numero di caratteri all'interno della stringa. Ad esempio:

```Swift
let stringa = "Ciao!"
print(stringa.count)
```

L'output di questo codice sarà 5, poiché la stringa è composta da 5 caratteri (C, i, a, o, !). Possiamo anche utilizzare il metodo `count` su stringhe vuote o stringhe contenenti soltanto spazi bianchi, ottenendo sempre un risultato pari a 0.

```Swift
let stringaVuota = ""
let stringaSpazi = "        "
print(stringaVuota.count) // Output: 0
print(stringaSpazi.count) // Output: 0
```

È importante notare che questo metodo conta anche gli emoji come un unico carattere, indipendentemente dal numero di code units utilizzate per rappresentarli. Ad esempio, il seguente codice restituirà 1 come output, nonostante l'emoji sia composto da 2 caratteri:

```Swift
let emoji = "😊"
print(emoji.count) // Output: 1
```

Invece, se vogliamo contare il numero di code units, possiamo utilizzare il metodo `unicodeScalars.count` nella seguente maniera:

```Swift
print(emoji.unicodeScalars.count) // Output: 2
```

## Deep Dive

Come accennato in precedenza, il metodo `count` conta gli emoji come un unico carattere, anche se possono essere composti da più di una code unit. Questo perché Swift utilizza il formato di codifica UTF-8 per rappresentare le stringhe, il che significa che ogni carattere può essere rappresentato da qualunque numero di code units. Questo può portare a situazioni in cui la lunghezza di una stringa può essere diversa da ciò che ci aspettiamo.

Un altro aspetto importante da considerare è il fatto che alcuni caratteri, come ad esempio le lettere accentate, vengono rappresentati in UTF-8 utilizzando più di una code unit. Pertanto, quando utilizziamo il metodo `count` su una stringa contenente tali caratteri, otterremo un risultato diverso dal numero di caratteri effettivi all'interno della stringa. Ad esempio:

```Swift
let frase = "Ciao a tutti!"
print(frase.count) // Output: 14
```

Anche se la frase contiene 12 caratteri, la presenza della lettera "à" comporta l'utilizzo di una code unit in più, aumentando la lunghezza totale a 14.

In caso di necessità, possiamo controllare la lunghezza di una stringa in base al numero di caratteri effettivi utilizzando il metodo `characters.count`, che elimina le code units aggiuntive utilizzate per rappresentare alcuni caratteri speciali.

## Vedere anche

- [Official Documentation for String.count in Swift](https://developer.apple.com/documentation/swift/string/2999974-count)
- [Swift Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Working with Strings in Swift](https://www.hackingwithswift.com/sixty/2/1/working-with-strings-in-swift)