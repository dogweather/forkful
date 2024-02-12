---
title:                "Sammanslagning av strängar"
date:                  2024-01-20T17:35:38.406582-07:00
model:                 gpt-4-1106-preview
simple_title:         "Sammanslagning av strängar"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?
Att slå ihop strängar, eller konkatenering, innebär att du sätter samman två eller flera textsträngar till en. Vi gör det för att skapa dynamisk text, som användarnamn i en hälsning eller värden i en loggfil.

## How to:
Swift gör att konkatenera strängar ser ut som barnlek. Här är det enkelt:

```Swift
let firstName = "Erik"
let lastName = "Svensson"
let fullName = firstName + " " + lastName
print(fullName)
```
Output: `Erik Svensson`

Använder du string interpolation blir det ännu smidigare:

```Swift
let city = "Stockholm"
let message = "Välkommen till \(city)!"
print(message)
```
Output: `Välkommen till Stockholm!`

Och om du har en samling av strängar:

```Swift
let words = ["Swift", "är", "skoj!"]
let sentence = words.joined(separator: " ")
print(sentence)
```
Output: `Swift är skoj!`

## Deep Dive:
Konkatenering är en grundläggande funktion som funnits sedan programmeringens barndom. I Swift kan strängar plus (+) operatorn användas för att enkelt sätta ihop strängar. Men, detta är inte alltid effektivt för stora datamängder, då varje operation skapar en ny sträng.

Alternativet, som är snällare mot minnet, är att använda `append`-metoden eller string interpolation. Dessutom kan vi använda `joined(separator:)`-metoden för att slå ihop element från en `Array`.

Till exempel:

```Swift
var story = "Det var en gång"
story.append(contentsOf: " en programmerare som älskade Swift.")
print(story)
```
Output: `Det var en gång en programmerare som älskade Swift.`

Det här är effektivare än att skapa nya strängar varje gång för längre eller komplexa textkonstruktioner.

## See Also:
- [Swift String Documentation](https://developer.apple.com/documentation/swift/string)
- [Swift Programming Language Guide](https://docs.swift.org/swift-book/GuidedTour/GuidedTour.html#ID466)
