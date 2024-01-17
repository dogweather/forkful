---
title:                "Att använda reguljära uttryck"
html_title:           "Swift: Att använda reguljära uttryck"
simple_title:         "Att använda reguljära uttryck"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Reguljära uttryck är ett sätt för programmerare att söka och manipulera textsträngar på ett strukturerat sätt. Det är särskilt användbart för att hitta specifika mönster eller uttryck i en lång text. Programmerare använder reguljära uttryck för att effektivt bearbeta och hantera stora mängder data.

## Såhär:

Ett grundläggande sätt att använda reguljära uttryck i Swift är med hjälp av metoden `matches` på en sträng. Till exempel, om vi vill söka efter alla förekomster av bokstaven "a" i en sträng, kan vi använda följande kod:

```Swift
let str = "Det var en gång en ko som hette Anna"
let matches = str.matches(for: "a")

print(matches) // ["a", "a", "a", "a"]
```

Vi kan också använda reguljära uttryck för att hitta mer komplicerade mönster. Till exempel, om vi vill hitta alla email-addresser i en text, kan vi använda följande reguljära uttryck:

```Swift
let str = "Kontakta mig på john.doe@example.com eller jane_smith@mail.com"
let pattern = "[a-z0-9._%+-]+@[a-z0-9.-]+\\.[a-z]{2,}"
let matches = str.matches(for: pattern)

print(matches) // ["john.doe@example.com", "jane_smith@mail.com"]
```

## Fördjupning:

Reguljära uttryck har funnits sedan 1950-talet och används i många olika programmeringsspråk. Det finns också alternativ till reguljära uttryck, såsom strängmanipulationsfunktioner eller andra sökmetoder.

I Swift används reguljära uttryck genom att använda strukturen `NSRegularExpression`. Det finns också många olika metoder för att söka och manipulera text med hjälp av reguljära uttryck, t.ex. `replacingMatches` och `rangeOfFirstMatch`.

## Se även:

- [Apple Developer dokumentation om Regular Expressions](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/Strings/Articles/CreatingStringsUsingFormatSpecifiers.html#//apple_ref/doc/uid/TP40004265)
- [RegExr - Verktyg för att testa reguljära uttryck](https://regexr.com/)