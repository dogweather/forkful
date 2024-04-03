---
date: 2024-01-20 17:35:38.406582-07:00
description: "Att sl\xE5 ihop str\xE4ngar, eller konkatenering, inneb\xE4r att du\
  \ s\xE4tter samman tv\xE5 eller flera textstr\xE4ngar till en. Vi g\xF6r det f\xF6\
  r att skapa dynamisk text, som\u2026"
lastmod: '2024-03-13T22:44:38.243336-06:00'
model: gpt-4-1106-preview
summary: "Att sl\xE5 ihop str\xE4ngar, eller konkatenering, inneb\xE4r att du s\xE4\
  tter samman tv\xE5 eller flera textstr\xE4ngar till en."
title: "Sammanslagning av str\xE4ngar"
weight: 3
---

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
