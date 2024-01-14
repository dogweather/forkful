---
title:                "Swift: Extrahera delsträngar"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför: Anledningen till att extrahera substrängar

Att extrahera substrängar är ett vanligt förekommande problem i Swift-programmering. Det kan behövas för att söka efter specifika ord eller tecken i en sträng, eller för att manipulera data på ett mer precist sätt. Genom att lära sig hur man extraherar substrängar kan du öka din förmåga att effektivt hantera strängar i dina program.

## Hur man gör det:

För att extrahera en substräng från en sträng i Swift, kan man använda funktionen `substring`. Detta görs genom att ange den önskade startpositionen och längden på den önskade substrängen:

```Swift
let sträng = "Hej jag heter Sven"
let nySubstring = sträng.substring(from: 8, length: 5)
print(nySubstring) // Resultat: "Sven"
```

Det är viktigt att notera att startpositionen i funktionen `substring` börjar på index 0, precis som i arrays i Swift. Det betyder att i exemplet ovan är "S" startpositionen 8, även om det är det 9:e tecknet i den ursprungliga strängen.

Om du istället vill extrahera en substräng från en specifik position till slutet av strängen, kan du använda funktionen `substring(from:)` och bara ange startpositionen:

```Swift
let andraSubstring = sträng.substring(from: 8)
print(andraSubstring) // Resultat: "Sven"
```

Det är också möjligt att extrahera substrängar baserat på första och sista förekomsten av ett specifikt tecken. För att göra detta, använd funktionen `substring(with: Range<String.Index>)` och ange en range av tecken att extrahera:

```Swift
let förstaTecknet = sträng.index(of: " ")!
let sistaTecknet = sträng.index(of: "j")!
let tredjeSubstring = sträng.substring(with: förstaTecknet..<sistaTecknet)
print(tredjeSubstring) // Resultat: "jag heter"
```

Man kan också utnyttja Swifts "string interpolation" för att extrahera enkla delar av en sträng baserat på specifika villkor. Till exempel:

```Swift
let Ålder = 29
let mening = "Jag är \(Ålder) år gammal"
let ÅldersSubstring = mening.substring(from: 7)
print(ÅldersSubstring) // Resultat: "29 år gammal"
```

Det finns många situationer där det kan vara användbart att kunna extrahera substrängar på detta sätt. Experimentera med olika kombinationer av funktionerna `substring` och `index` och se vad du kan åstadkomma!

## Djupdykning:

Att kan vara användbart att förstå hur Swift hanterar strängar på en underliggande nivå när man arbetar med substrängar. I Swift är strängar representerade av strukturer, eller "structs" på engelska. Detta innebär att de är värden, inte referenser. Det betyder i princip att en sträng är en unik instans och vid en extraktion av en substräng, skapas en ny sträng instans med bara önskade delen av den ursprungliga strängen.

En annan viktig sak att notera är att Swifts strängar är Unicode-kompatibla, vilket innebär att de kan innehålla tecken från många olika språk och skriftsystem. Detta gör det möjligt att extrahera substrängar baserat på Unicode-egenskaper som "gruppering" av tecken eller diakritiska tecken.

## Se också:

-[Swift.org: Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
-[Hacking with Swift: How to store substring in a string](https://www.hackingwithswift.com/example-code/strings/how-to-store-a-substring-in-a-string)
-[Swift by Sundell: Working with dates and times in Swift](https://www.swiftbysundell.com/basics/working-with-dates-and-times-in-swift/)