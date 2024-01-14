---
title:                "Swift: Kapitalisering av en sträng"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför

I Swift-programmering är det ofta nödvändigt att kunna manipulera och formatera text. En vanlig uppgift är att ändra från små bokstäver till versaler eller vice versa. I den här bloggposten kommer vi att titta närmare på hur vi kan göra just detta i Swift, och varför det är användbart.

## Så här gör du

För att ändra en sträng till versaler i Swift, används metoden `uppercased()` som tillhör strängklassen. Enkelt uttryckt, metoden tar en sträng och returnerar en ny version av den med alla bokstäver som är stora. Här är ett exempel:

```Swift
let str = "hej alla från Sverige"
print(str.uppercased())
```

Output:

```
HEJ ALLA FRÅN SVERIGE
```

För att göra om strängen till små bokstäver används istället metoden `lowercased()`. Låt oss ta en titt på ett annat exempel:

```Swift
let name = "HANNAH"
print(name.lowercased())
```

Output:

```
hannah
```

## Djupdykning

Att kunna formatera texten på detta sätt kan vara väldigt användbart, till exempel när man vill göra sökningar i en sträng oberoende av bokstavens storlek. Detta är särskilt användbart när man håller på med användarsökningar eller filtreringar.

Ytterligare en användning av att göra om en sträng till versaler eller små bokstäver är för att göra output enhetlig, oavsett hur användaren matar in sin information. Detta hjälper till att undvika fel och förvirring.

## Se även

Vill du lära dig mer om hur man manipulerar text i Swift? Kolla in dessa resurser:

- [Officiell Swift-dokumentation om strängar](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Tutorial: How to Use String Manipulation in Swift](https://www.appcoda.com/swift-string-manipulation/)

Lycka till med ditt Swift-programmerande!