---
title:                "Extrahering av substringar"
html_title:           "Swift: Extrahering av substringar"
simple_title:         "Extrahering av substringar"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför

Ibland när man arbetar med strängar i Swift så kan man behöva ta ut en del av strängen, även kallat en substring. Det kan vara användbart för att få ut specifika delar av en lång sträng eller för att manipulera och modellera data på ett mer effektivt sätt.

## Så här gör du

För att kunna extrahera en substring från en befintlig sträng i Swift, kan man använda sig av metoden `substring(from:)` eller `substring(to:)`. Metoden `substring(from:)` tar en parameter som indikerar vilken position i strängen som substringen ska börja på. Till exempel:

```Swift
let str = "Hej, jag heter Johanna"
let substr = str.substring(from: 10)
print(substr) // kommer att skriva ut "Johanna"
```

Metoden `substring(to:)` däremot tar en parameter som indikerar var i strängen som substringen ska sluta. Till exempel:

```Swift
let str = "Hej, jag heter Johanna"
let substr = str.substring(to: 7)
print(substr) // kommer att skriva ut "Hej, ja"
```

Man kan också använda båda metoder tillsammans för att extrahera en del av en sträng mellan två positioner. Till exempel:

```Swift
let str = "apple, banana, orange"
let substr = str.substring(from: 7, to: 13)
print(substr) // kommer att skriva ut "banana"
```

Det finns också en annan metod som heter `substring(with:)` som tar ett `Range` som parameter för att definiera vilken del av strängen som ska extraheras. Till exempel:

```Swift
let str = "abcdefg"
let range = Range(uncheckedBounds: (3, 6)) // Range som definierar strängen "def"
let substr = str.substring(with: range)
print(substr) // kommer att skriva ut "def"
```

När man extraherar en substring från en sträng i Swift så returneras alltid en ny sträng, vilket är viktigt att komma ihåg vid vidare manipulation av data.

## Deep Dive

Förutom de olika metoderna för att extrahera substrings som nämnts ovan finns det också möjlighet att använda sig av regular expressions för att definiera vilka delar av en sträng som ska extraheras. Detta kan vara användbart när man vill ta ut delar av en sträng baserat på specifika mönster. Detta görs genom att använda metoden `range(of:options:)", där `options` är en `enum` som innehåller olika alternativ för hur sökningen ska utföras.

```Swift
let str = "This is a string. It contains some numbers: 123456"
let range = str.range(of: "[0-9]+", options: .regularExpression)
let substr = str.substring(with: range!)
print(substr) // kommer att skriva ut "123456"
```

Man kan också använda sig av `NSString` i Swift för att använda mer komplicerade funktioner för att extrahera substrings. Till exempel:

```Swift
let str = "This is a string"
let nsString = str as NSString
let substr = nsString.substring(with: NSRange(location: 5, length: 2))
print(substr) // kommer att skriva ut "is"
```

## Se även

- [String Subscripting](https://developer.apple.com/documentation/swift/1557855-string_subscripting)
- [NSRange](https://developer.apple.com/documentation/foundation/nsrange)
- [Regular expressions in Swift](https://www.hackingwithswift.com/articles/108/regular-expressions-in-swift)