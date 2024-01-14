---
title:    "Swift: Estrazione di sottostringhe"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Perché

Esistono situazioni in cui è necessario estrarre una parte di una stringa più grande. Questo può essere utile quando si deve manipolare una stringa in modo specifico o quando si desidera semplicemente visualizzare solo una parte di essa. In Swift, esistono molte opzioni per estrarre una sottostringa da una stringa principale. Vediamo come farlo nella sezione successiva.

## Come fare

Per estrarre una sottostringa in Swift, utilizziamo il metodo `substring` disponibile su un tipo di dato `String`. In questo metodo, dobbiamo fornire l'indice di inizio e la lunghezza della sottostringa che vogliamo estrarre. Ad esempio:

```Swift
let stringa = "Ciao a tutti!"
let sottostringa = stringa.substring(from: 2, length: 5)
print(sottostringa) // output: 'o a t'
```

In questo esempio, abbiamo estratto una sottostringa a partire dall'indice 2 (cioè il terzo carattere) della stringa principale e con una lunghezza di 5 caratteri.

Possiamo anche utilizzare l'indice di fine invece della lunghezza per estrarre una sottostringa. In questo caso, il metodo `substring` calcolerà automaticamente la lunghezza della sottostringa. Ad esempio:

```Swift
let nuovaSottostringa = stringa.substring(from: 6, to: 10)
print(nuovaSottostringa) // output: 'tut!'
```

In questo esempio, stiamo estraendo una sottostringa dall'indice 6 al carattere 10 (escluso) della stringa principale.

È anche possibile utilizzare il metodo `prefix` per estrarre una sottostringa dalle prime lettere della stringa principale o il metodo `suffix` per estrarre una sottostringa dalle ultime lettere. Ecco un esempio che utilizza entrambi i metodi:

```Swift
let prefisso = stringa.prefix(4)
print(prefisso) // output: 'Ciao'

let suffisso = stringa.suffix(6)
print(suffisso) // output: 'tutti!'
```

## Deep Dive

Oltre ai metodi menzionati sopra, esistono anche altre opzioni per estrarre stringhe in Swift, come il metodo `substring(with:)` che utilizza un intervallo di indici come parametro o il metodo `components(separatedBy:)` che suddivide una stringa in una matrice di sottostringhe utilizzando un delimitatore specificato.

È importante notare che, a partire da Swift 4, il tipo `String` ha subito alcune modifiche significative e quindi, se si sta utilizzando una versione precedente di Swift, può essere necessario utilizzare un approccio leggermente diverso per estrarre sottostringhe.

## Vedi anche

- [Documentazione ufficiale di Swift su estrazione delle stringhe](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID293)
- [Tutorial su estrazione delle stringhe in Swift](https://www.hackingwithswift.com/articles/141/how-to-use-strings-in-swift)