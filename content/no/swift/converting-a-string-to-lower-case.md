---
title:                "Konvertere en streng til små bokstaver"
date:                  2024-01-20T17:39:19.733796-07:00
model:                 gpt-4-1106-preview
simple_title:         "Konvertere en streng til små bokstaver"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?
Når du konverterer en streng til små bokstaver, endrer du alle bokstavene til deres miniatyr versjoner. Programmerere gjør dette for å forenkle sammenligninger og søk, siden store og små bokstaver da betraktes likt.

## How to:
Swift gjør det lett å håndtere strenger. Her er hvordan du konverterer:

```Swift
let originalString = "Hei, Norge!"
let lowercasedString = originalString.lowercased()

print(lowercasedString)
// Output: "hei, norge!"
```

Enkelt som en pølse i brød.

## Deep Dive:
I de gamle C-dagene håndterte man strenger som char-arrays. Å gjøre dem små igjen var en manuell prosess av å iterere og konvertere hver bokstav for seg selv - langsommelig og feilutsatt.

Swifts `.lowercased()` er en del av `String` klassen, en høy-nivå fasilitet som skjuler krybbedetaljene. Det støtter Unicode og språkspesifikke bokstaver (tenk Æ, Ø og Å).

Alternativt, hvis du trenger mer kontroll, kan du bruke `Locale` for å håndtere kulturspesifikke tilfeller som tyrkisk, der "I" blir til "ı" i stedet for "i".

```Swift
let turkishString = "İSTANBUL"
let lowercasedTurkish = turkishString.lowercased(with: Locale(identifier: "tr_TR"))

print(lowercasedTurkish)
// Output: "istanbul"
```

## See Also:
- Swift Standard Library: https://developer.apple.com/documentation/swift/string/2296181-lowercased
- Swift String og Character: https://developer.apple.com/documentation/swift/string
- Locale: https://developer.apple.com/documentation/foundation/locale
