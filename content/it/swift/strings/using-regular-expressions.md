---
aliases:
- /it/swift/using-regular-expressions/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:17.203446-07:00
description: "Le espressioni regolari, o regex, sono sequenze di caratteri che formano\
  \ un modello di ricerca, spesso utilizzate per compiti di confronto o manipolazione\u2026"
lastmod: 2024-02-18 23:08:56.203187
model: gpt-4-0125-preview
summary: "Le espressioni regolari, o regex, sono sequenze di caratteri che formano\
  \ un modello di ricerca, spesso utilizzate per compiti di confronto o manipolazione\u2026"
title: Utilizzo delle espressioni regolari
---

{{< edit_this_page >}}

## Cosa e Perché?
Le espressioni regolari, o regex, sono sequenze di caratteri che formano un modello di ricerca, spesso utilizzate per compiti di confronto o manipolazione di stringhe. I programmatori le utilizzano per varie operazioni, dalla validazione dei dati all'analisi, fino alle trasformazioni, rendendole uno strumento indispensabile nel processo e nella manipolazione del testo attraverso vari linguaggi di programmazione, inclusi Swift.

## Come Fare:
Il supporto nativo di Swift per le regex utilizza la classe `NSRegularExpression`, insieme ai metodi di intervallo e sostituzione della classe String. Di seguito è riportato un esempio dell'uso delle regex per trovare ed evidenziare gli indirizzi email all'interno di un blocco di testo:

```swift
import Foundation

let text = "Contattaci su support@example.com o feedback@example.org per ulteriori informazioni."
let regexPattern = "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}"

do {
    let regex = try NSRegularExpression(pattern: regexPattern)
    let matches = regex.matches(in: text, range: NSRange(text.startIndex..., in: text))

    if !matches.isEmpty {
        for match in matches {
            let range = Range(match.range, in: text)!
            print("Trovato: \(text[range])")
        }
    } else {
        print("Nessuna corrispondenza trovata.")
    }
} catch {
    print("Errore Regex: \(error.localizedDescription)")
}

// Esempio di Output:
// Trovato: support@example.com
// Trovato: feedback@example.org
```

Per scenari più complessi o focalizzati sulla comodità, puoi utilizzare librerie di terze parti come SwiftRegex, che semplifica la sintassi e amplia le possibilità. Sebbene la libreria standard di Swift sia potente, alcuni sviluppatori preferiscono queste librerie per la loro sintassi concisa e funzionalità aggiuntive. Ecco come potresti svolgere un compito simile utilizzando una libreria di terze parti ipotetica:

```swift
// Assumendo che esista una libreria chiamata SwiftRegex e sia importata
let text = "Contattaci su hello@world.com o visita il nostro sito web."
let emailPattern = "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}"

let emails = text.matches(for: emailPattern) // Metodo ipotetico fornito da SwiftRegex
if emails.isEmpty {
    print("Nessun indirizzo email trovato.")
} else {
    emails.forEach { email in
        print("Trovato: \(email)")
    }
}

// Output ipotetico assumendo che il metodo `matches(for:)` esista in SwiftRegex:
// Trovato: hello@world.com
```

Questo esempio illustra l'utilizzo di un pacchetto di espressioni regolari di terze parti per semplificare il ritrovamento delle corrispondenze all'interno di una stringa, assumendo che esistano metodi di comodità come `matches(for:)`. È importante fare riferimento alla documentazione della rispettiva libreria di terze parti per una sintassi precisa e la disponibilità dei metodi.
