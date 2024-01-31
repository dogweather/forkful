---
title:                "Ta bort citattecken från en sträng"
date:                  2024-01-26T03:42:02.072834-07:00
model:                 gpt-4-0125-preview
simple_title:         "Ta bort citattecken från en sträng"

category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att ta bort citattecken från en sträng innebär att ta bort alla citatmarkeringar som omsluter innehållet. Vi gör detta för att rensa indata, förbereda data för lagring eller bli av med onödig textformatering som kan störa databehandlingen.

## Hur man gör:

Swift låter dig ta hand om uppgiften att ta bort citattecken ganska lätt. Här är ett snabbt exempel med `replacingOccurrences(of:with:)`, som gör precis vad det låter som—byter ut bitar av text mot något annat, eller ingenting alls.

```swift
var quotedString = "\"Detta är en 'citerad' sträng.\""
let unquotedString = quotedString.replacingOccurrences(of: "\"", with: "")
print(unquotedString) // Detta är en 'citerad' sträng.

// Hantera enkla citattecken? Bara ändra söktermen.
quotedString = "'Här är ett annat exempel.'"
let singleQuoteRemoved = quotedString.replacingOccurrences(of: "'", with: "")
print(singleQuoteRemoved) // Här är ett annat exempel.
```

Resultatet blir strängar utan citattecken helt redo för vad du än har planerat härnäst.

## På Djupet

Vi har "städat upp" strängar som dessa sedan programmeringens gryning. I de tidiga dagarna handlade det mer om att spara värdefullt minne och undvika syntaxfel vid indatahantering. Spola fram till idag, och det handlar om god datahygien—speciellt när man hanterar JSON eller förbereder strängar för databasarbete. Ett vilsamt citattecken kan sätta stopp för SQL-förfrågningar snabbare än du kan säga "syntaxfel".

Alternativ? Tja, om du tycker att `replacingOccurrences(of:with:)` är lite för vanilj, kanske du vill gräva ner dig i reguljära uttryck för mer komplexa mönster eller när du vill ta bort citattecken endast på vissa ställen. Swifts `NSRegularExpression` klass är din vän här. Men kom ihåg, regex kan vara ett tveeggat svärd—kraftfullt men ibland överdrivet.

Utförandet, `replacingOccurrences(of:with:)` är en metod som tillhandahålls av `String` i Swift, som internt anropar mer komplexa strängmanipuleringsfunktioner som hanterar Unicode och andra intrikatesser i modern textbehandling. Det är en av de där "enkla på ytan, komplexa under huven" grejerna som Swift hanterar så att du inte behöver.

## Se även

För mer om strängmanipulationer i Swift:

- Swifts programmeringsspråk (Strängar och Tecken): [Swift.org Dokumentation](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- NSRegularExpression: [Apple Utvecklardokumentation](https://developer.apple.com/documentation/foundation/nsregularexpression)

Och om du nu är nyfiken på reguljära uttryck och vill testa dina mönster:

- Regex101: [Regex Tester och Debugger](https://regex101.com)
