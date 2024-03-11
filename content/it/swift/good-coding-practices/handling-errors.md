---
date: 2024-01-26 00:57:58.471263-07:00
description: "Gestire gli errori in Swift significa anticipare e rispondere ai problemi\
  \ che emergono quando il codice \xE8 in esecuzione. Lo facciamo per controllare\
  \ il\u2026"
lastmod: '2024-03-11T00:14:17.394240-06:00'
model: gpt-4-1106-preview
summary: "Gestire gli errori in Swift significa anticipare e rispondere ai problemi\
  \ che emergono quando il codice \xE8 in esecuzione. Lo facciamo per controllare\
  \ il\u2026"
title: Gestione degli errori
---

{{< edit_this_page >}}

## Cosa e Perché?
Gestire gli errori in Swift significa anticipare e rispondere ai problemi che emergono quando il codice è in esecuzione. Lo facciamo per controllare il caos—impedendo alle app di bloccarsi e fornendo all'utente un'esperienza fluida.

## Come fare:
Swift utilizza la gestione degli errori con i blocchi `do`, `try` e `catch`. Diamo un'occhiata:

```Swift
enum FileError: Error {
    case fileNonEsiste
    case nessunPermesso
}

func leggiFile(alPercorso path: String) throws -> String {
    // Facciamo finta di avere una logica qui per controllare se un file esiste e se abbiamo il permesso di leggerlo
    let fileEsiste = false
    let hoPermesso = true

    if !fileEsiste {
        throw FileError.fileNonEsiste
    }

    if !hoPermesso {
        throw FileError.nessunPermesso
    }

    return "Il contenuto del file va qui"
}

do {
    let contenutoFile = try leggiFile(alPercorso: "/percorso/al/file")
    print(contenutoFile)
} catch FileError.fileNonEsiste {
    print("Ops! File non trovato.")
} catch FileError.nessunPermesso {
    print("Ah! Nessun permesso per leggere il file.")
} catch {
    print("Si è verificato un errore sconosciuto.")
}

```

Output di Esempio:

```
Ops! File non trovato.
```

## Approfondimento
La gestione degli errori non è sempre stata così sofisticata come lo è ora. In Objective-C, si doveva gestire con puntatori a oggetti NSError, il che sembrava goffo. Ora, abbiamo un sistema più elegante con gli enum di Swift e il protocollo `Error`.

Il `throw` di Swift ci consente di segnalare che qualcosa è andato storto. I blocchi `do` agiscono come domini consapevoli degli errori, il prefisso `try` chiama le operazioni rischiose, e `catch` gestisce le cose se vanno male.

Le opzionali sono un'alternativa per situazioni che non sono proprio di "stato di errore" ma potrebbero comunque non avere "nessun risultato". Sono un po' come le variabili di Schrödinger—hanno un valore o non lo hanno.

Per una comprensione più approfondita, esaminare i tipi `Result`, che sono ibridi raffinati tra modelli di ritorno regolari e schemi di errore.

## Vedi Anche
- Guida ufficiale alla gestione degli errori in Swift: [Documenti Apple](https://docs.swift.org/swift-book/LanguageGuide/ErrorHandling.html)
- Migliori pratiche per la gestione degli errori in Swift: [RayWenderlich.com](https://www.raywenderlich.com/1851-beginning-swift-error-handling)
- Gestione avanzata degli errori in Swift: [Articolo su Medium](https://medium.com/better-programming/advanced-error-handling-in-swift-4f6bdf6b01d8)
