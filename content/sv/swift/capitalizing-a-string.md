---
title:    "Swift: Att stora bokstavera en sträng"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

##Varför?

Att kapitalisera en sträng, eller göra om den till stora bokstäver, kan vara en viktig del av en Swift-utvecklares verktygslåda. Det kan hjälpa till att göra texten mer läsbar och enhetlig, vilket i sin tur kan förbättra användarupplevelsen och minska möjligheten för fel och buggar.

##Så här gör du

För att enkelt kapitalisera en sträng i Swift använder man sig av funktionen `uppercased()`. Detta exempel visar hur man kapitaliserar en sträng som heter "hej":

```
let str = "hej"
let capitalizedStr = str.uppercased()

print(capitalizedStr) // Output: HEJ
```

För strängar som redan är i kapitaliserat format, eller för att växla mellan stor och liten bokstav, kan man istället använda sig av funktionen `lowercased()`. Detta exempel visar hur man växlar mellan stor och liten bokstav för strängen "Hej":

```
let str = "Hej"
let lowercasedStr = str.lowercased()

print(lowercasedStr) // Output: hej
```

##Djupdykning

Som utvecklare är det alltid viktigt att förstå vad som händer bakom kulisserna för de funktioner vi använder. I Swift är det vanligtvis en kombination av olika metoder och egenskaper som utför kapitaliseringen. `uppercased()` använder sig av `Cocoa.framework`, medan `lowercased()` använder sig av `Foundation.framework`.

För att få en bättre förståelse för hur man kapitaliserar en sträng i Swift, rekommenderar vi att du utforskar dokumentationen för `NSString` och `NSMutableString`. Dessa klasser tillhandahåller flera metoder som är nyttiga för att manipulera textsträngar.

##Se även

- [Apple's offentliga dokument för Swift](https://developer.apple.com/documentation/swift)
- [Officiell Swift-sida på Swift.org](https://swift.org/)
- [Swift-kurs på Codecademy](https://www.codecademy.com/learn/learn-swift)