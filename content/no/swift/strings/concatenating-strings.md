---
title:                "Sammenslåing av strenger"
aliases: - /no/swift/concatenating-strings.md
date:                  2024-01-20T17:36:25.929319-07:00
model:                 gpt-4-1106-preview
simple_title:         "Sammenslåing av strenger"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?
I Swift smelter vi sammen strenger for å skape nye setninger eller setningselementer. Vi gjør det blant annet for å dynamisk generere tekst, som brukerinput eller data fra en server.

## How to:
```Swift
// Enkel sammenføyning
let greeting = "Hei, "
let name = "Ola!"
let welcome = greeting + name
print(welcome) // Output: Hei, Ola!

// Sammenføyning med interpolering
let age = 30
let birthdayGreeting = "Gratulerer med \(age)-årsdagen!"
print(birthdayGreeting) // Output: Gratulerer med 30-årsdagen!

// Sammenføyning med append funksjonen
var message = "Dette er"
message.append(" en setning.")
print(message) // Output: Dette er en setning.
```

## Deep Dive
I de tidlige dagene av programmering var strengmanipulasjon mer manuelt og klossete. Maskinspråk og tidlig høy-nivå programmeringsspråk krevde nøye håndtering av minneplassering og bufferstørrelser for å unngå krasjer. 

Swift og andre moderne språk forenkler drastisk string-sammenføyning med operasjoner som `+`, `+=`, og string interpolasjon (innsetting av variabler rett i strengene). 

Interpolering er særlig kraftfullt fordi det lar deg sette inn tall, beregninger eller andre variabler rett i strengen uten ekstra konvertering. Swift sikrer at resultatet av hva enn du setter inn, blir en streng, og det gjør koden mer lesbar.

Her er det viktig å ikke forveksle med `NSMutableString` fra Objective-C som er forgjengeren til Swift. Mens Swifts strenger er verdi-typer, var `NSMutableString` en referanse-type, noe som medførte forskjeller i behandling av minne og ytelse.

## See Also
- Swift Dokumentasjon om Strenger: [Apple Developer Documentation](https://developer.apple.com/documentation/swift/string)
- Swift String og Karakterer: [Swift Programming Language Guide](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
