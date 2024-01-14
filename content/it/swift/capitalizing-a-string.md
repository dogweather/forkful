---
title:    "Swift: Capitalizzazione di una stringa"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

#Perché

Capitalizzare una stringa è un'operazione comune nella programmazione Swift. Ciò è particolarmente utile quando si desidera migliorare la leggibilità di una stringa o quando si deve rispettare una determinata convenzione di scrittura.

#Come fare

Per capitalizzare una stringa in Swift, è possibile utilizzare il metodo `uppercased()` su un'istanza di `String`. Vediamo un esempio:

```Swift
let frase = "ciao a tutti"
let fraseCapitalizzata = frase.uppercased()

print(fraseCapitalizzata)
```

Questo codice restituirà l'output "CIAO A TUTTI".

In alternativa, è anche possibile utilizzare il metodo `capitalized` per capitalizzare la prima lettera di ogni parola in una stringa. Ad esempio:

```Swift
let frase = "ciao a tutti"
let fraseCapitalizzata = frase.capitalized

print(fraseCapitalizzata)
```

L'output sarà "Ciao A Tutti".

#Approfondimento

Ci sono alcune cose importanti da tenere in mente quando si utilizzano questi metodi per capitalizzare una stringa:

- I metodi non modificano la stringa originale, ma ne restituiscono una nuova con la modifica applicata.
- Se la stringa non contiene alcuna lettera, ad esempio solo numeri o caratteri speciali, i metodi restituiranno semplicemente la stessa stringa.
- Se ci sono caratteri accentati o caratteri speciali nella stringa, essi verranno mantenuti durante la capitalizzazione.

Inoltre, è importante notare che esistono anche altri metodi per capitalizzare una stringa, come ad esempio `lowercased()` per renderla tutta minuscola o `localizedCapitalized` per rispettare le convenzioni linguistiche della lingua selezionata.

#Vedi anche

- [Documentazione Apple su `uppercased()` ](https://developer.apple.com/documentation/swift/string/2894568-uppercased)
- [Documentazione Apple su `capitalized()`](https://developer.apple.com/documentation/swift/string/2894200-capitalized)