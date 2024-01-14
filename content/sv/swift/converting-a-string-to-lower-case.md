---
title:                "Swift: Konvertering av en sträng till små bokstäver"
simple_title:         "Konvertering av en sträng till små bokstäver"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

Hej alla Swift-entusiaster! Idag ska vi prata om hur du kan konvertera en sträng till små bokstäver i Swift. Varför skulle du vilja göra det? Hur gör man det? Och vad händer under ytan? Låt oss gräva djupare och ta reda på det!

## Varför

Att konvertera en sträng till små bokstäver kan vara användbart när du behöver söka igenom och jämföra textsträngar. Genom att ha en gemensam form (allt i små bokstäver) blir det enklare att hitta matchningar och göra jämförelser. Det är också användbart för att presentera text på ett enhetligt sätt, oavsett hur användaren har skrivit in det.

## Hur man gör det

För att enkelt konvertera en sträng till små bokstäver i Swift kan du använda metoden `lowercased()` på strängen. Här är ett exempel:

```Swift
let text = "HELLO EVERYONE"
let lowercaseText = text.lowercased()
print(lowercaseText)
```

Output: `hello everyone`

Som du kan se returneras nu strängen i små bokstäver. Här är några fler exempel på hur man kan använda denna metod:

```Swift
let text = "welcome to Sweden"
let anotherText = "Welcome to Sweden"

print(text.lowercased()) // output: welcome to sweden
print(anotherText.lowercased()) // output: welcome to sweden
```

Observera att både "Welcome" och "welcome" är konverterade till "welcome". Överkurs: om du behöver jämföra två strängar men vill ignorera skillnader i stor bokstav kan du använda `caseInsensitiveCompare()` metoden.

```Swift
let firstString = "HeLlO"
let secondString = "hello"

if firstString.caseInsensitiveCompare(secondString) == .orderedSame {
    print("Strängarna är lika!")
} else {
    print("Strängarna är olika.")
}
```

Output: `Strängarna är lika!`

## Djupdykning

Nu när vi har sett hur man konverterar en sträng till små bokstäver, låt oss dyka lite djupare och titta på vad som händer under ytan. I Swift används Unicode för att representera tecken. Varje bokstav har en unik kodpunkt som motsvarar den, vilket möjliggör att stödja en mängd olika språk. När `lowercased()` anropas på en sträng används Unicode för att konvertera alla bokstäver till deras motsvarande små bokstäver.

## Se även

Här är några användbara länkar för att lära dig mer om Swift-strängar och Unicode:

- [Strings and Characters in Swift](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Unicode på Swift för Alla](https://swiftforall.com/articles/strings-and-characters-on-swift-part-5-unicode/)
- [Sträng-manipulering på Swift by John Sundell](https://www.swiftbysundell.com/articles/strings-in-swift/)

Vi hoppas att du har lärt dig något nytt om strängar och Unicode i Swift. Lycka till med dina kodprojekt och ha det så kul med Swift!