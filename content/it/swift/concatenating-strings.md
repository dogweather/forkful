---
title:                "Swift: Concatenazione di stringhe"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché

Concatenare le stringhe è un'operazione comune nella programmazione Swift, che consente di unire più stringhe in una sola. Questo può essere utile per creare output leggibili, costruire URL o formattare correttamente i dati prima di inviarli a un server. In questa guida vedremo come farlo in modo semplice ed efficiente.

## Come fare

Per concatenare le stringhe in Swift, è possibile utilizzare l'operatore `+` o il metodo `append()` della classe `String`. Entrambi i metodi sono altrettanto validi, ma l'utilizzo di `append()` può essere più vantaggioso se si desidera concatenare un grande numero di stringhe.

Un esempio di utilizzo dell'operatore `+` potrebbe essere il seguente:

```Swift
var greeting = "Ciao"
var name = "Maria"
var message = greeting + ", " + name + "!"
print(message)
```

Questo codice produrrà in output la stringa "Ciao, Maria!", unendo le variabili `greeting` e `name` alla stringa ", ".

Per utilizzare il metodo `append()` invece, possiamo sfruttare i vantaggi di un ciclo `for-in` per concatenare un numero variabile di stringhe. Ad esempio:

```Swift
var names = ["Maria", "Luca", "Giovanni"]
var message = "Ciao"
for name in names {
    message.append(", " + name)
}
print(message + "!")
```

In questo caso, il risultato in output sarà la stringa "Ciao, Maria, Luca, Giovanni!".

## Approfondimento

La classe `String` in Swift dispone di molti metodi utili per la manipolazione e la gestione delle stringhe. Ad esempio, il metodo `contains()` permette di verificare se una stringa contiene un'altra stringa specificata. Questo può essere utile per controllare se una stringa inserita dall'utente è presente in un certo elenco.

Un altro metodo utile è `replacingOccurrences()`, che consente di sostituire parte di una stringa con un'altra. Ad esempio, è possibile sostituire tutti gli spazi in una stringa con un carattere specifico utilizzando il seguente codice:

```Swift
var text = "Questo è un esempio di stringa."
var replacedText = text.replacingOccurrences(of: " ", with: "-")
print(replacedText)
```

Il risultato in output sarà "Questo-è-un-esempio-di-stringa.".

## Vedi anche

Per ulteriori informazioni sulle stringhe in Swift, consulta la [documentazione ufficiale del linguaggio](https://developer.apple.com/documentation/swift/string) o [questo articolo](https://medium.com/@abhimuralidharan/ultimate-guide-to-string-manipulation-in-swift-c92172dc2ce9).