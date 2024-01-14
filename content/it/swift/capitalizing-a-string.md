---
title:                "Swift: Capitalizzazione di una stringa"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché

Spesso in programmazione ci troviamo a dover manipolare stringhe di testo per renderle più leggibili o uniformi. Una delle operazioni comuni è la capitalizzazione di una stringa, ovvero mettere la prima lettera di ogni parola in maiuscolo. In questo articolo vedremo come eseguire questa operazione in Swift e perché potrebbe essere utile farlo.

## Come Fare

Per capitalizzare una stringa in Swift possiamo utilizzare il metodo `capitalized` della classe `String`. Ecco un esempio di codice che mostra come utilizzare questo metodo:

```Swift
let testo = "programmazione in swift"
let testoCapitalizzato = testo.capitalized
print(testoCapitalizzato) // Stampa "Programmazione In Swift"
```

Come si può vedere, il metodo `capitalized` ha trasformato la stringa in maiuscolo, mettendo la prima lettera di ogni parola in maiuscolo. È importante notare che questo metodo non solo capitalizza le prime lettere, ma anche tutte le altre lettere che erano già in maiuscolo rimangono tali.

Inoltre, possiamo specificare il locale da utilizzare per la capitalizzazione passando un parametro al metodo `capitalized`. Ad esempio, se vogliamo capitalizzare una stringa utilizzando il locale tedesco, possiamo fare così:

```Swift
let testo = "programmazione in swift"
let testoCapitalizzato = testo.capitalized(locale: Locale(identifier: "de_DE"))
print(testoCapitalizzato) // Stampa "Programmierung In Swift"
```

È importante notare che i risultati possono variare in base al locale utilizzato.

## Approfondimento

Oltre al metodo `capitalized`, Swift ci offre altre opzioni per capitalizzare una stringa. Ad esempio, possiamo utilizzare il metodo `capitalized(with: Locale)` della classe `NSMutableString` per capitalizzare una stringa modificandola direttamente:

```Swift
var testo = "programmazione in swift"
(testo as NSString).capitalized(with: Locale(identifier: "it_IT")) // Modifica la stringa
print(testo) // Stampa "Programmazione In Swift"
```

Inoltre, possiamo utilizzare la classe `TitleCaseConverter` della libreria `Foundation` per capitalizzare una stringa secondo le regole del case del titolo. Ad esempio:

```Swift
let testo = "programmazione in swift"
let titleCaseConverter = TitleCaseConverter()
let testoCapitalizzato = titleCaseConverter.convert(testo) // "Programmazione in Swift"
```

È importante notare che questa classe non gestisce correttamente le parole composte, quindi nel caso di parole composte con la prima lettera già in maiuscolo, queste verranno comunque capitalizzate.

## Vedi Anche

- [Manuale di Swift](https://docs.swift.org/swift-book/)
- [Documentazione ufficiale di Apple su `String`](https://developer.apple.com/documentation/swift/string)
- [Informazioni sui locale di Swift](https://developer.apple.com/documentation/foundation/locale)