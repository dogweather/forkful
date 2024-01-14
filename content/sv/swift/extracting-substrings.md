---
title:    "Swift: Extrahera delsträngar"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Varför

Idag ska vi prata om en grundläggande, men otroligt användbar, funktion i Swift-programmering - att extrahera substrängar. Substrängar är delar av en sträng som kan användas för att manipulera och bearbeta text på ett enkelt sätt. Låt oss titta närmare på varför det är viktigt och hur man gör det i Swift!

## Hur man gör det

För att extrahera en substräng från en befintlig sträng, kan du använda funktionen `substring (with: Range)` i Swift. Låt oss anta att vi har en sträng som heter `helloWorld` och vi vill få tag på ordet "hello" från den. Vi kan använda följande kod för att göra det:

```Swift
let helloWorld = "helloWorld"
let hello = helloWorld.substring(with: 0..<5)
print(hello) // Output: hello
```

I det här exemplet använder vi `substring (with: Range)` -funktionen för att extrahera en del av vår `helloWorld` -sträng. Vi använder sedan en halvöppen räckvidd för att ange vilka delar av strängen vi vill ha. I detta fall vill vi ha de första fem tecknen, dvs "hello".

## Djupdykning

Nu har vi sett hur vi kan extrahera en enkel substräng från en sträng. Men det finns mycket mer vi kan göra med substrängar i Swift. Här är några saker att tänka på:

- Du kan också använda funktionen `substring (from:)` för att extrahera en substräng från en viss position till slutet av strängen.
- För att hitta en viss substräng i en sträng kan du använda funktionen `range (of:)`.
- Om du vill ersätta en del av en sträng med en annan substräng kan du använda funktionen `replacingOccurrences (of: with:)`.

Det finns många fler sätt att använda substrängar i Swift, men dessa exempel ger dig en bra utgångspunkt för hur du kan använda dem i dina egna projekt.

## Se även

Här är några resurser som kan vara användbara för att lära dig mer om substrängar och andra Swift-funktioner:

- [Swift Programming Language dokumentation](https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html)
- [Swift API-dokumentation](https://developer.apple.com/documentation/swift)
- [Swift Playgrounds](https://www.apple.com/swift/playgrounds/)

Tack för att du läste! Nu är du redo att ta dina Swift-programmeringskunskaper till nästa nivå genom att utnyttja substrängar. Lycka till!