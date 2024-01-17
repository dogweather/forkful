---
title:                "Estrazione di sottostringhe"
html_title:           "Swift: Estrazione di sottostringhe"
simple_title:         "Estrazione di sottostringhe"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

### Che cos'è e perché è importante?

Estrarre le sottostringhe, o porzioni di testo più piccole, da una stringa più grande è un'operazione comune nella programmazione. I programmatori lo fanno per ottenere solo le informazioni necessarie da una stringa di testo più grande o per manipolare i dati in modo più efficiente.

### Come fare:

Estrarre una sottostringa in Swift è semplice usando il metodo `substring()` su una stringa. Di seguito è riportato un esempio di codice che mostra come ottenere il primo nome da una stringa contenente sia il nome che il cognome.

```Swift
let nomeCompleto = "Mario Rossi"
let primoNome = nomeCompleto.substring(to: nomeCompleto.index(of: " ")!)
print(primoNome)
```

Questo codice stamperà "Mario". In questo esempio, `nomeCompleto.index(of: " ")` restituisce l'indice della prima occorrenza dello spazio nella stringa, che viene utilizzato per estrarre la sottostringa fino a quel punto.

### Approfondimento:

L'estrazione delle sottostringhe è stata resa più semplice con l'introduzione di `substring()` in Swift. In passato, i programmatori dovevano usare metodi più complicati come `NSString.substringWithRange()` per ottenere le sottostringhe. Inoltre, esistono altre opzioni per estrarre le sottostringhe come `prefix()` e `suffix()`, che possono essere utili in situazioni diverse.

È importante notare che quando un'altra variabile fa riferimento alla stessa stringa originale, qualsiasi modifica apportata alla sottostringa ottenuta da quella stringa si riflette anche nella stringa originale.

### Vedi anche:

- [Documentazione ufficiale di Apple su `substring()`](https://developer.apple.com/documentation/swift/string/2961145-substring)
- [Guida alla programmazione di Swift di Apple](https://developer.apple.com/library/archive/documentation/Swift/Conceptual/Swift_Programming_Language/CollectionTypes.html#//apple_ref/doc/uid/TP40014097-CH8-ID105)