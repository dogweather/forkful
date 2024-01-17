---
title:                "Maiuscolando una stringa"
html_title:           "Swift: Maiuscolando una stringa"
simple_title:         "Maiuscolando una stringa"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Che cos'è & Perché?

Capitalizzare una stringa significa trasformare la prima lettera di ogni parola in maiuscolo. È una pratica comune tra i programmatori perché rende il testo più leggibile e coerente.

## Come procedere:

```
let stringa = "questo è un esempio di stringa da capitalizzare"
let stringaCapitalizzata = stringa.capitalized

print(stringaCapitalizzata) // Questo È Un Esempio Di Stringa Da Capitalizzare
```

Puoi anche forzare la capitalizzazione di tutte le lettere usando il metodo `uppercased` e cambiare solo la prima lettera usando il metodo `capitalizingFirstLetter`.

```
let stringa = "capitalizzazione forzata"
let stringaInMaiuscolo = stringa.uppercased()
let stringaCapitalizzata = stringa.capitalizingFirstLetter()

print(stringaInMaiuscolo) // CAPITALIZZAZIONE FORZATA
print(stringaCapitalizzata) // Capitalizzazione forzata
```

## Approfondimento:

La pratica di capitalizzare le stringhe ha origini lontane nella tipografia e nell'editoria. Molte lingue, come l'inglese e l'italiano, hanno convenzioni di scrittura che richiedono l'uso delle maiuscole in determinati contesti, ad esempio all'inizio di una frase o dei nomi propri. Inoltre, in alcuni casi, capitalizzare una stringa può facilitare il riconoscimento delle parole chiave all'interno del codice.

Ci sono anche alternative per capitalizzare una stringa, come usare il metodo `capitalized(with: Locale?)`, che prende in input un oggetto Locale e lo utilizza per decidere come capitalizzare la stringa in base alle regole linguistiche della lingua specificata. Inoltre, puoi anche implementare le tue funzioni personalizzate per capitalizzare le stringhe, adattate alle tue esigenze specifiche.

## Vedi anche:

Per ulteriori informazioni sulla capitalizzazione delle stringhe in Swift, puoi consultare la [documentazione ufficiale di Apple](https://developer.apple.com/documentation/swift/string/1777391-capitalized) o [questo articolo](https://www.hackingwithswift.com/example-code/strings/how-to-capitalize-the-first-letter-of-a-string) di Hacking with Swift.