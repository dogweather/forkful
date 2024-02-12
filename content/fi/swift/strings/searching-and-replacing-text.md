---
title:                "Tekstin etsiminen ja korvaaminen"
aliases:
- /fi/swift/searching-and-replacing-text.md
date:                  2024-01-20T17:58:54.829623-07:00
model:                 gpt-4-1106-preview
simple_title:         "Tekstin etsiminen ja korvaaminen"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?
Mikä ja miksi? Tekstin etsiminen ja korvaaminen tarkoittaa merkkijonojen hakuja ja niiden muuttamista ohjelmassa. Ohjelmoijat käyttävät tätä päivittäen tekstidataa ja automatisoiden tylsiä tehtäviä.

## How to:
```Swift
let originalString = "Omena on vihreä. Omena on maukas."
let searchString = "Omena"
let replacementString = "Banaani"

let replacedString = originalString.replacingOccurrences(of: searchString, with: replacementString)
print(replacedString)
```
Tuloste:
```
Banaani on vihreä. Banaani on maukas.
```

Lisäesimerkki säännöllisillä lausekkeilla:
```Swift
import Foundation

let regexOriginalString = "Kukat 001, Kukat 002, Kukat 003."
let regexPattern = "Kukat \\d{3}"

if let regex = try? NSRegularExpression(pattern: regexPattern, options: []) {
    let newString = regex.stringByReplacingMatches(in: regexOriginalString,
                                                   options: [],
                                                   range: NSRange(0..<regexOriginalString.utf16.count),
                                                   withTemplate: "Kasvit")
    print(newString)
}
```
Tuloste:
```
Kasvit, Kasvit, Kasvit.
```

## Deep Dive
Historia: Tekstin etsimis- ja korvaustoiminnot ovat keskeisiä työkaluja ohjelmoinnissa jo vuosikymmenten ajan. Unix-pohjaisissa järjestelmissä työkalut kuten `sed` ovat tehneet tätä komentorivillä.

Vaihtoehdot: Swiftissä `replacingOccurrences`-metodi tekee perusvaihdot. Säännöllisten lausekkeiden kanssa, `NSRegularExpression` mahdollistaa monimutkaisemmat etsintä- ja korvauskuviot.

Toteutus: `replacingOccurrences` käyttää yksinkertaista merkkijonojen vertailua. `NSRegularExpression` hyödyntää säännöllisiä lausekkeita, jotka ovat voimakas mutta monimutkainen tapa suorittaa tekstihakuja ja -muunnoksia.

## See Also
- Swift-dokumentaatio `String`: https://developer.apple.com/documentation/swift/string
- NSRegularExpression-dokumentaatio: https://developer.apple.com/documentation/foundation/nsregularexpression
- Säännöllisten lausekkeiden opas: https://www.regular-expressions.info/
- `sed`-komennon opas Unixissa: https://www.gnu.org/software/sed/manual/sed.html
