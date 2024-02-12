---
title:                "Capitalizzare una stringa"
aliases:
- /it/swift/capitalizing-a-string/
date:                  2024-02-03T19:06:31.363892-07:00
model:                 gpt-4-0125-preview
simple_title:         "Capitalizzare una stringa"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa & Perché?

Capitalizzare una stringa in Swift modifica la stringa data in modo che il suo primo carattere sia maiuscolo, e i caratteri rimanenti minuscoli. I programmatori fanno questo per scopi come la formattazione di nomi o frasi secondo regole grammaticali o standard dell'interfaccia utente.

## Come fare:

Le strutture `String` di Swift vengono fornite con un paio di metodi integrati per manipolare il caso delle stringhe. Ecco alcuni approcci per capitalizzare le stringhe in Swift, inclusi l'uso di metodi standard e di librerie di terze parti se necessario.

### Usando metodi integrati

Per capitalizzare la prima lettera di una stringa e mettere in minuscolo il resto:

```swift
let myString = "hello, world"
let capitalizedString = myString.prefix(1).uppercased() + myString.dropFirst().lowercased()
print(capitalizedString) // Output: "Hello, world"
```

Per capitalizzare la prima lettera di ogni parola in una frase, puoi usare la proprietà `capitalized`:

```swift
let sentence = "hello, world"
let capitalizedSentence = sentence.capitalized
print(capitalizedSentence) // Output: "Hello, World"
```

### Usando una libreria di terze parti

Anche se la libreria standard di Swift è piuttosto completa, alcuni formati di capitalizzazione specifici potrebbero richiedere operazioni più complesse o possono essere semplificati utilizzando librerie di terze parti. Una delle più popolari per la manipolazione delle stringhe è SwiftRichString. (Nota: Assicurati sempre di includere le librerie di terze parti tramite Swift Package Manager, CocoaPods o Carthage, e di importarle nel tuo file.)

Prima, dovresti aggiungere `SwiftRichString` al tuo progetto. Una volta installato, puoi usarlo per eseguire varie operazioni sulle stringhe, includendo esigenze di capitalizzazione specifiche. Tuttavia, ad oggi, i metodi integrati di Swift coprono adeguatamente la maggior parte dei casi d'uso di capitalizzazione senza la necessità di librerie esterne solo per capitalizzare le stringhe.

Fai sempre riferimento all'ultima documentazione della libreria per eventuali aggiornamenti o modifiche nei metodi.
