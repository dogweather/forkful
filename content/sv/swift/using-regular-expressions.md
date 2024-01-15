---
title:                "Användning av reguljära uttryck"
html_title:           "Swift: Användning av reguljära uttryck"
simple_title:         "Användning av reguljära uttryck"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Varför

Alla som kodar i Swift och behöver hantera strängar och mönster på ett effektivt sätt, bör lära sig om reguljära uttryck. Reguljära uttryck (även kallat regex) är ett kraftfullt verktyg för att söka, matcha och modifiera text på ett flexibelt sätt.

## Så här

Att använda reguljära uttryck i Swift är enkelt och kan ge stora fördelar. Här är några enkla exempel för att visa hur man kan använda regex i Swift:

```Swift
// Skapa ett reguljärt uttryck för att matcha ett telefonnummer
let phoneNumberRegex = try! NSRegularExpression(pattern: "^\\d{3}-\\d{3}-\\d{4}$", options: .caseInsensitive)

// Utför matchande på en sträng
let phoneNumber = "123-456-7890"
let range = NSRange(location: 0, length: string.count)
let isMatch = phoneNumberRegex.firstMatch(in: phoneNumber, options: [.anchored], range: range) != nil

// Utmatning: true
print(isMatch)
```

Reguljära uttryck kan också användas för att ersätta delar av en sträng:

```Swift
// Ersätt alla siffror i en sträng med stjärnor
let creditCardNumber = "1234567890123456"
let maskedString = creditCardNumber.replacingOccurrences(of: "[0-9]", with: "*", options: .regularExpression, range: nil)

// Utmatning: ************3456
print(maskedString)
```

Det finns många fler möjligheter med reguljära uttryck i Swift, så var inte rädd för att experimentera och testa nya saker!

## Djupdykning

Reguljära uttryck består av en serie tecken och specialtecken som matchar specifika mönster i en sträng. De är mycket kraftfulla, men samtidigt också komplext, så det kan ta lite tid att förstå dem helt och hållet.

I Swift, är reguljära uttryck implementerade med hjälp av klassen NSRegularExpression och dess metoder. Det finns också många användbara metoder från Sträng- och Text-protokollen som gör det enkelt att använda reguljära uttryck utan att behöva skapa en instans av NSRegularExpression.

Om du vill lära dig mer om reguljära uttryck och hur du kan använda dem i Swift, finns det många bra bloggar och tutorials där ute som kan hjälpa dig på vägen.

## Se också

- [Official Swift Regular Expression Documentation](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [Regex Cheat Sheet](https://www.rexegg.com/regex-quickstart.html)
- [Swift by Sundell tutorial on regular expressions](https://www.swiftbysundell.com/articles/regular-expressions-in-swift/)
- [NSHipster article on regular expressions](https://nshipster.com/nsregularexpression/)