---
title:                "Eliminazione di caratteri che corrispondono a un pattern"
aliases:
- /it/swift/deleting-characters-matching-a-pattern.md
date:                  2024-01-20T17:43:00.924306-07:00
model:                 gpt-4-1106-preview
simple_title:         "Eliminazione di caratteri che corrispondono a un pattern"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why?
Rimuovere caratteri corrispondenti a un pattern significa filtrare una stringa eliminando tutti i caratteri o gruppi di caratteri che rientrano in un criterio specifico. I programmatori lo fanno per pulire i dati, per validazione o per formattare l'output in modo coerente.

## How to:
Utilizzeremo `NSRegularExpression` per trovare pattern nei caratteri e `String` methods per eliminarli:

```Swift
import Foundation

func deleteMatchingCharacters(from string: String, pattern: String) -> String {
    let regex = try! NSRegularExpression(pattern: pattern)
    let range = NSRange(location: 0, length: string.utf16.count)
    return regex.stringByReplacingMatches(in: string, options: [], range: range, withTemplate: "")
}

let originalString = "Ciao, io sono il Developer123!"
let cleanedString = deleteMatchingCharacters(from: originalString, pattern: "\\d")

print(cleanedString) // Output: "Ciao, io sono il Developer!"
```

Ecco, caratteri indesiderati spariti. Ricorda di gestire gli errori nella vita reale, non usare `try!`.

## Deep Dive
In Swift, `NSRegularExpression` ha ereditato il suo posto dalla suite di strumenti di Objective-C. Le regex sono un modo potente ma complesso per cercare pattern nei testi, e abusarne può portare a prestazioni lente. 

Un'alternativa? `String` methods come `filter` o `replacingOccurrences`. Sono più semplici da usare ma meno potenti. Il context storico ci mostra che la scelta dipendeva dal linguaggio: le regex dominavano in Perl, meno in C. Oggi, in Swift, abbiamo scelte.

Dettagli di implementazione? Le regex utilizzano automi e espressioni regolari teoriche. Capirli richiede tempo ma ti rende un mago delle stringhe!

## See Also
- [NSRegularExpression Documentation](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [Swift String Documentation](https://developer.apple.com/documentation/swift/string)
- [Lezione sulle Regular Expressions](https://www.raywenderlich.com/5765-regular-expressions-tutorial-getting-started)
