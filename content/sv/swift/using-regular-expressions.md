---
title:                "Använda reguljära uttryck"
date:                  2024-01-19
html_title:           "Bash: Använda reguljära uttryck"
simple_title:         "Använda reguljära uttryck"

category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Användning av reguljära uttryck är matchning av textmönster. Programmerare använder dem för att validera, extrahera eller ersätta text, vilket effektiviserar och automatiserar textbearbetning.

## How to:
```Swift
import Foundation

let text = "Kontakta oss på info@example.se eller 08-123 45 67."
let emailPattern = "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}"
let phonePattern = "\\d{2}-\\d{3}\\s\\d{2}\\s\\d{2}"

if let emailRange = text.range(of: emailPattern, options: .regularExpression),
   let phoneRange = text.range(of: phonePattern, options: .regularExpression) {
    let email = String(text[emailRange])
    let phone = String(text[phoneRange])
    print("E-post hittad: \(email)")
    print("Telefonnummer hittat: \(phone)")
}
```
Sample output:
```
E-post hittad: info@example.se
Telefonnummer hittat: 08-123 45 67
```

## Deep Dive
Reguljära uttryck (regex) har funnits sedan 1950-talet, skapade av matematikern Stephen Kleene. Swift använder ICU:s (International Components for Unicode) regex-motor. Alternativ till regex är String-metoder som .contains eller .split men de erbjuder inte samma flexibilitet. Vid implementation, var medveten om regex kan vara resource-intensive; överanvändning bör undvikas för prestanda.

## See Also
- [NSRegularExpression Apple Documentation](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [Swift.org String and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [RegexOne: Learning Regular Expressions with Interactive Exercises](https://regexone.com/)
