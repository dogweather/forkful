---
date: 2024-01-20 17:39:19.733796-07:00
description: "N\xE5r du konverterer en streng til sm\xE5 bokstaver, endrer du alle\
  \ bokstavene til deres miniatyr versjoner. Programmerere gj\xF8r dette for \xE5\
  \ forenkle\u2026"
lastmod: '2024-03-13T22:44:41.129286-06:00'
model: gpt-4-1106-preview
summary: "N\xE5r du konverterer en streng til sm\xE5 bokstaver, endrer du alle bokstavene\
  \ til deres miniatyr versjoner."
title: "Konvertere en streng til sm\xE5 bokstaver"
weight: 4
---

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
