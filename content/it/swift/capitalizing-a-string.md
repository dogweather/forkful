---
title:                "Maiuscolizzare una stringa"
date:                  2024-01-19
html_title:           "Bash: Maiuscolizzare una stringa"
simple_title:         "Maiuscolizzare una stringa"

category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Capitalizzare una stringa significa convertire tutte le lettere in maiuscolo. I programmatori lo fanno per uniformare il testo, per enfasi o per confrontare stringhe in modo case-insensitive.

## How to:
Ecco come capitalizzare una stringa in Swift:

```Swift
let fraseMinusc = "ciao mondo"
let fraseMaiusc = fraseMinusc.uppercased()
print(fraseMaiusc)
```

Output:
```
CIAO MONDO
```

E un'esempio su come capitalizzare solo la prima lettera di ogni parola:

```Swift
let titolo = "benvenuti al tutorial swift"
let titoloCapitalizzato = titolo.capitalized
print(titoloCapitalizzato)
```

Output:
```
Benvenuti Al Tutorial Swift
```

## Deep Dive
La capitalizzazione delle stringhe viene da lontano. Inizialmente utilizzata nei testi per distingue nomi propri, sigle, ecc., ora aiuta i programmatori a gestire l'input degli utenti e l'interfaccia utente. 

Alternativamente, in Swift, potresti voler trasformare solo la prima lettera di una stringa in maiuscolo. Non c'è una funzione built-in per questo, quindi tocca a te:

```Swift
extension String {
    func capitalizeFirstLetter() -> String {
        return prefix(1).uppercased() + dropFirst()
    }
}

let parola = "esempio"
print(parola.capitalizeFirstLetter())
```

Output:
```
Esempio
```

Per quanto riguarda l'implementazione, `uppercased()` e `capitalized` sono metodi forniti da Swift sul tipo `String`. Usano regole specifiche di localizzazione (es. l'inglese usa maiuscole all'inizio di ogni parola nei titoli, l'italiano no).

## See Also
Per maggiori dettagli su come manipolare le stringhe in Swift:

- La documentazione ufficiale di Swift sulle stringhe: [Swift String](https://developer.apple.com/documentation/swift/string)
- Un'introduzione alla programmazione in Swift su: [Swift.org - Guided Tour](https://docs.swift.org/swift-book/GuidedTour/GuidedTour.html)
- Informazioni aggiuntive sulle estensioni e personalizzazioni in Swift: [Swift String Customization](https://www.hackingwithswift.com/articles/58/swift-extensions-every-ios-developer-should-know)
