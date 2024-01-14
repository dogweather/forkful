---
title:    "Swift: Unione di stringhe"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché

La concatenazione di stringhe è un'operazione comune nella programmazione Swift che consente di unire più stringhe in una singola stringa più lunga. Questo può essere utile per la creazione di messaggi dinamici, l'elaborazione di input dell'utente o la formattazione di output.

## Come fare

Per concatenare due o più stringhe, è possibile utilizzare l'operatore "+" o il metodo `append` sull'oggetto `String`. Ad esempio:

```Swift
let nome = "Paolo"
let saluto = "Ciao " + nome + "!"
print(saluto)
```

Questo produrrà l'output: `Ciao Paolo!`.

Il metodo `append` permette anche di aggiungere una stringa alla fine di un'altra stringa esistente:

```Swift
var frase = "Mi piace il"
frase.append(" gelato")
print(frase)
```

Questo produrrà l'output: `Mi piace il gelato`.

È anche possibile utilizzare l'operatore di assegnazione `+=` per concatenare una stringa a una variabile già esistente:

```Swift
var numero = 42
numero += " è il mio numero preferito"
print(numero)
```

Questo produrrà l'output: `42 è il mio numero preferito`.

## Approfondimento

Nella programmazione Swift, ogni stringa è in realtà un'istanza di una struttura chiamata `String`. Questa struttura implementa il protocollo `Collection` che fornisce il metodo `append` per aggiungere elementi alla fine della stringa.

Inoltre, il metodo `append` è definito come mutating, il che significa che modifica direttamente la stringa originale invece di crearne una nuova ogni volta che viene chiamato. Questo è importante da tenere a mente quando si lavora con stringhe più complesse.

## Vedi anche

- [Documentazione Apple su String](https://developer.apple.com/documentation/swift/string)
- [Tutorial di concatenazione di stringhe in Swift](https://www.hackingwithswift.com/example-code/strings/how-to-join-strings-to-make-a-single-string)