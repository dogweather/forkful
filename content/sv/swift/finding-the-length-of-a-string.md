---
title:                "Hitta längden på en sträng"
date:                  2024-01-20T17:48:26.603962-07:00
model:                 gpt-4-1106-preview
simple_title:         "Hitta längden på en sträng"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att hitta längden på en sträng innebär att ta reda på antalet tecken den innehåller. Programmerare behöver ofta veta detta för att validera indata, manipulera text, eller bara för att hantera olika textbaserade uppgifter effektivt.

## Hur gör man:
För att få längden på en sträng i Swift är det bara att använda `count`-egenskapen:

```Swift
let greetings = "Hej!"
let length = greetings.count
print(length) // 4
```

Enkel som en plätt. Observera att Swift räknar karaktärer på ett sätt som tar hänsyn till tecken som kan vara sammansatta, såsom emoji eller accenter:

```Swift
let flag = "🇸🇪"
print(flag.count) // 1
```

## Djupdykning
Från då och då har behandlingen av stränglängder ändrats i programmeringsspråk. I vissa tidiga språk, kunde man ta fram längden genom att räkna tills man stötte på en speciell 'null'-karaktär. Swift, och moderna språk, hanterar strängar på ett säkrare och mer internationellt sätt genom att använda Unicode-skalära värden, vilket gör `count` mer tillförlitlig över olika språk och tecken.

Ett alternativ till `count` är att arbeta med `utf16.count` eller `utf8.count`, vilket kan vara relevant om du behöver den specifika längden i UTF-16 eller UTF-8 kodning för till exempel nätverksöverföring eller lagring.

Så här fungerar det under huven: Swifts `String` är en samling av `Character` värden, där varje `Character` kan representera flera Unicode-skalära värden. Det innebär att Swift tar hänsyn till grafemkluster — grupper av en eller flera skalära värden som tillsammans representerar ett enda mänskligt läsbart tecken.

## Se även
- Swift-dokumentation för `String`: [https://developer.apple.com/documentation/swift/string](https://developer.apple.com/documentation/swift/string)
- Unicode-konsortiets hemsida för grunderna i Unicode-tecken: [https://home.unicode.org/basic-info/overview/](https://home.unicode.org/basic-info/overview/)
- Apple's Swift blogg för en djupare förståelse av `String` och karaktärsanalys: [https://developer.apple.com/swift/blog/?id=30](https://developer.apple.com/swift/blog/?id=30)