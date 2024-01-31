---
title:                "Säännöllisten lausekkeiden käyttö"
date:                  2024-01-19
simple_title:         "Säännöllisten lausekkeiden käyttö"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?

Regular expressions (regex) ovat tapa etsiä kaavoja tekstistä. Ohjelmoijat käyttävät niitä tekstin prosessointiin, validointiin ja hakutulosten jalostamiseen.

## How to:

```Swift
import Foundation

let testString = "Hello, world! 123456"
let regexPattern = "\\b\\w+\\b"

// Luodaan regular expression
if let regex = try? NSRegularExpression(pattern: regexPattern) {
    // Etsitään kaikki täsmäävät tulokset
    let matches = regex.matches(in: testString, range: NSRange(testString.startIndex..., in: testString))
    
    // Tulostetaan löydetyt tulokset
    for match in matches {
        if let range = Range(match.range, in: testString) {
            print(testString[range])
        }
    }
}

// Output:
// Hello
// world
// 123456
```

## Deep Dive

Regular expressions juontavat juurensa teoreettiseen tietojenkäsittelytieteeseen, 1950-luvulle. Vaihtoehtoja regexille ovat mm. merkkijonojen manuaalinen käsittely tai parserit. Swiftin `NSRegularExpression`-luokan taustalla on ICU-kirjasto, joka on yleinen C-ohjelmointikielen regex-kirjasto.

## See Also

- Swift-turorialit ["NSRegularExpression"](https://developer.apple.com/documentation/foundation/nsregularexpression)
- ICU User Guide Regex-osio ["ICU User Guide"](https://unicode-org.github.io/icu/userguide/strings/regexp.html)
