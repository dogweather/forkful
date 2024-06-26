---
date: 2024-01-20 17:58:54.829623-07:00
description: "How to: Historia: Tekstin etsimis- ja korvaustoiminnot ovat keskeisi\xE4\
  \ ty\xF6kaluja ohjelmoinnissa jo vuosikymmenten ajan. Unix-pohjaisissa j\xE4rjestelmiss\xE4\
  \u2026"
lastmod: '2024-04-05T22:38:57.505288-06:00'
model: gpt-4-1106-preview
summary: "Historia: Tekstin etsimis- ja korvaustoiminnot ovat keskeisi\xE4 ty\xF6\
  kaluja ohjelmoinnissa jo vuosikymmenten ajan. Unix-pohjaisissa j\xE4rjestelmiss\xE4\
  \ ty\xF6kalut kuten `sed` ovat tehneet t\xE4t\xE4 komentorivill\xE4. Vaihtoehdot:\
  \ Swiftiss\xE4 `replacingOccurrences`-metodi tekee perusvaihdot. S\xE4\xE4nn\xF6\
  llisten lausekkeiden kanssa, `NSRegularExpression` mahdollistaa monimutkaisemmat\
  \ etsint\xE4- ja korvauskuviot. Toteutus: `replacingOccurrences` k\xE4ytt\xE4\xE4\
  \ yksinkertaista merkkijonojen vertailua. `NSRegularExpression` hy\xF6dynt\xE4\xE4\
  \ s\xE4\xE4nn\xF6llisi\xE4 lausekkeita, jotka ovat voimakas mutta monimutkainen\
  \ tapa suorittaa tekstihakuja ja -muunnoksia."
title: Tekstin etsiminen ja korvaaminen
weight: 10
---

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
