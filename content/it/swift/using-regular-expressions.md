---
title:                "Utilizzo delle espressioni regolari"
date:                  2024-01-19
html_title:           "Arduino: Utilizzo delle espressioni regolari"
simple_title:         "Utilizzo delle espressioni regolari"

category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Le espressioni regolari (Regex) filtrano e manipolano testo. I programmatori le usano per la potenza e flessibilità di ricerca e sostituzione nei dati testuali.

## How to:
Per applicare le regex in Swift, usa la classe `NSRegularExpression`. Qui sotto, un esempio per trovare e-mail in una stringa.

```Swift
import Foundation

let testo = "mandami una mail a mario.rossi@example.com o a luigi.bianchi@example.it"
let regex = try! NSRegularExpression(pattern: "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}", options: [])

let matches = regex.matches(in: testo, options: [], range: NSRange(testo.startIndex..., in: testo))

for match in matches {
    if let range = Range(match.range, in: testo) {
        let email = testo[range]
        print(email)
    }
}
```

Output:
```
mario.rossi@example.com
luigi.bianchi@example.it
```

## Deep Dive
Le regex risalgono agli anni '50 e sono state implementate in molti linguaggi. In Swift, NSRegularExpression segue ICU (International Components for Unicode), che differisce dal PCRE (Perl Compatible Regular Expressions) usato in altri linguaggi. Alternativamente, si hanno librerie come RegexKitLite per altre sintassi o prestazioni.

## See Also
- La documentazione ufficiale di Swift su NSRegularExpression: https://developer.apple.com/documentation/foundation/nsregularexpression
- Tutorial di Ray Wenderlich sulle regex in Swift: https://www.raywenderlich.com/5765-nsregularexpression-tutorial-getting-started
- ICU User Guide for Regular Expressions: http://userguide.icu-project.org/strings/regexp
