---
title:                "Swift: Att Kapitalisera en Sträng"
simple_title:         "Att Kapitalisera en Sträng"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför 

Att kapitalisera en sträng kan vara användbart när man vill ändra utseendet på en textsträng, till exempel om man vill visa en rubrik eller ett namn med stor första bokstav. 

## Så här gör du 

```Swift 
let str = "hej och välkommen"
print(str.capitalized) // Output: "Hej Och Välkommen"
```

Det enklaste sättet att kapitalisera en sträng i Swift är att använda metoden `capitalized`, vilken automatiskt omvandlar första bokstaven i varje ord till stor bokstav. Man kan också använda metoden `uppercased` för att göra hela strängen stor bokstav eller `lowercased` för att göra hela strängen små bokstäver. 

```Swift 
let str = "jag Älskar Att Programmera"
print(str.uppercased()) // Output: "JAG ÄLSKAR ATT PROGRAMMERA"
print(str.lowercased()) // Output: "jag älskar att programmera"
```

## Fördjupa dig

I Swift finns det flera metoder för att manipulera och ändra strängar. En av dessa är metoden `localizedCapitalized`, vilken tar hänsyn till språktillhörighet och kulturella normer när den kapitaliserar en sträng. Det går också att välja vilka tecken som ska anses som bokstäver genom att använda metoden `capitalized(with:)`. 

Det finns även möjlighet att använda `NSMutableString` för att ändra en sträng direkt utan att behöva skapa en ny sträng. Med hjälp av metoden `replaceCharacters(in:with:)` kan man till exempel byta ut vissa tecken med andra. 

## Se även

- [Apple Dokumentation om capitalizing strings](https://developer.apple.com/documentation/foundation/nsstring/1164307-capitalized)
- [Blogginlägg om olika sätt att ändra strängar i Swift](https://www.hackingwithswift.com/articles/126/capitalizing-strings-in-swift)