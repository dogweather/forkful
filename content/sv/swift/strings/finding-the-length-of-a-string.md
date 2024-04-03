---
date: 2024-01-20 17:48:26.603962-07:00
description: "Att hitta l\xE4ngden p\xE5 en str\xE4ng inneb\xE4r att ta reda p\xE5\
  \ antalet tecken den inneh\xE5ller. Programmerare beh\xF6ver ofta veta detta f\xF6\
  r att validera indata,\u2026"
lastmod: '2024-03-13T22:44:38.242407-06:00'
model: gpt-4-1106-preview
summary: "Att hitta l\xE4ngden p\xE5 en str\xE4ng inneb\xE4r att ta reda p\xE5 antalet\
  \ tecken den inneh\xE5ller."
title: "Hitta l\xE4ngden p\xE5 en str\xE4ng"
weight: 7
---

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
