---
title:                "Einsatz von regulären Ausdrücken"
date:                  2024-01-19
html_title:           "Bash: Einsatz von regulären Ausdrücken"
simple_title:         "Einsatz von regulären Ausdrücken"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Was & Warum?
Reguläre Ausdrücke sind Muster, um Text nach bestimmten Regeln zu durchsuchen und zu manipulieren. Programmierer nutzen sie, um komplexe Such- und Ersetzungsaufgaben effizient zu bewältigen.

## So geht's:
```Swift
import Foundation

let text = "Die Email-Adresse ist beispiel@example.com."
let regexPattern = "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,64}"

if let regex = try? NSRegularExpression(pattern: regexPattern, options: []) {
    let matches = regex.matches(in: text, options: [], range: NSRange(location: 0, length: text.utf16.count))
    
    if let match = matches.first {
        let range = Range(match.range, in: text)!
        let foundEmail = String(text[range])
        print(foundEmail)  // "beispiel@example.com"
    }
}
```

## Deep Dive
Reguläre Ausdrücke gibt es schon seit den 1950er Jahren, entwickelt von Stephen Cole Kleene. Alternativen zu regulären Ausdrücken sind spezielle Parsing-Methoden, die je nach Anwendungsfall effizienter sein können. Swift benutzt `NSRegularExpression`, eine Klasse, die auf der ICU-Bibliothek basiert, und bietet damit eine reichhaltige Syntax für Musterabgleiche.

## See Also
- Swift Standard Library: https://developer.apple.com/documentation/swift
- NSRegularExpression Dokumentation: https://developer.apple.com/documentation/foundation/nsregularexpression
- Reguläre Ausdrücke lernen: https://regexone.com/
