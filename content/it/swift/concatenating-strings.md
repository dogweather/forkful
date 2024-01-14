---
title:    "Swift: Concatenazione di stringhe"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Perché

Concatenare stringhe è una delle operazioni più comuni nella programmazione Swift. È essenziale per combinare diverse parole o frasi in una singola stringa. Questo può essere utile in molte situazioni, come stampare output personalizzato o costruire URL dinamici.

## Come Fare

Per concatenare stringhe in Swift, puoi utilizzare l'operatore `+`. Ad esempio, se vogliamo creare una stringa che combina il tuo nome e cognome, puoi scrivere:

```Swift
let nome = "Maria"
let cognome = "Rossi"
let nomeCompleto = nome + " " + cognome
print(nomeCompleto) // Maria Rossi
```

È importante notare che, quando si utilizza l'operatore `+` per concatenare stringhe, ogni componente deve essere una stringa. Se hai bisogno di un valore numerico o booleano, devi prima convertirlo in una stringa utilizzando il metodo `String()`.

```Swift
let punteggio = 95
let messaggio = "Il tuo punteggio finale è " + String(punteggio) + " su 100"
print(messaggio) // Il tuo punteggio finale è 95 su 100
```

Puoi anche utilizzare il metodo `append()` per aggiungere una stringa a un'altra. Ad esempio:

```Swift
var indirizzo = "Viale Italia"
indirizzo.append(", 25")
print(indirizzo) // Viale Italia, 25
```

## Approfondimento

In Swift, le stringhe sono considerate come collezioni di caratteri, quindi puoi utilizzare anche il metodo `joined()` per concatenare più stringhe in una sola. Ad esempio:

```Swift
let words = ["Ciao", "a", "tutti", "!"]
let phrase = words.joined(separator: " ")
print(phrase) // Ciao a tutti !
```

Puoi anche utilizzare gli operatori di assegnazione combinati come `+=` per concatenare stringhe in modo più conciso. Ad esempio:

```Swift
var messaggio = "Buongiorno"
messaggio += ", come stai?"
print(messaggio) // Buongiorno, come stai?
```

## Vedi Anche

Per ulteriori informazioni sulle stringhe in Swift, puoi consultare la documentazione ufficiale sul [tipo di dato String](https://developer.apple.com/documentation/swift/string) e sull'[opzione di interpolazione delle stringhe](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID293).