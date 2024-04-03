---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:34.692445-07:00
description: "Hur man g\xF6r: Swifts `String`-strukturer kommer med ett par inbyggda\
  \ metoder f\xF6r att manipulera str\xE4ngars versalisering. H\xE4r \xE4r n\xE5gra\
  \ tillv\xE4gag\xE5ngss\xE4tt f\xF6r\u2026"
lastmod: '2024-03-13T22:44:38.234796-06:00'
model: gpt-4-0125-preview
summary: "Swifts `String`-strukturer kommer med ett par inbyggda metoder f\xF6r att\
  \ manipulera str\xE4ngars versalisering."
title: "G\xF6r om en str\xE4ng till versaler"
weight: 2
---

## Hur man gör:
Swifts `String`-strukturer kommer med ett par inbyggda metoder för att manipulera strängars versalisering. Här är några tillvägagångssätt för att göra första bokstaven i en sträng stor i Swift, inklusive användning av standardmetoder och tredjepartsbibliotek om nödvändigt.

### Använda inbyggda metoder
För att göra första bokstaven i en sträng stor och göra resten gemena:

```swift
let myString = "hej, världen"
let capitalizedString = myString.prefix(1).uppercased() + myString.dropFirst().lowercased()
print(capitalizedString) // Utdata: "Hej, världen"
```

För att göra första bokstaven i varje ord i en mening stor, kan du använda egenskapen `capitalized`:

```swift
let sentence = "hej, världen"
let capitalizedSentence = sentence.capitalized
print(capitalizedSentence) // Utdata: "Hej, Världen"
```

### Använda ett tredjepartsbibliotek
Även om Swifts standardbibliotek är ganska omfattande, kan vissa specifika versaliseringar kräva mer komplexa operationer eller kan förenklas med hjälp av tredjepartsbibliotek. Ett av de populära för strängmanipulation är SwiftRichString. (Notera: Se alltid till att inkludera tredjepartsbibliotek genom Swift Package Manager, CocoaPods eller Carthage, och importera dem i din fil.)

Först behöver du lägga till `SwiftRichString` i ditt projekt. När det är installerat kan du använda det för att utföra olika strängoperationer, inklusive specifika behov av versalisering. Dock, som det är nu, täcker Swifts inbyggda metoder adekvat de flesta fall av versalisering utan behovet av externa bibliotek endast för att göra bokstäver stora.

Referera alltid till det senaste dokumentationen av biblioteket för eventuella uppdateringar eller ändringar i metoder.
