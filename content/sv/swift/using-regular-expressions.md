---
title:                "Swift: Användning av regelbundna uttryck"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Varför

När vi programmerar vill vi ofta hantera och manipulera text. Ibland behöver vi hitta specifika mönster i en textsträng eller ersätta delar av den med annan text. Här kommer reguljära uttryck, även kända som "regular expressions", till vår hjälp. De är ett kraftfullt verktyg för att söka, matcha och manipulera text.

## Så här använder du reguljära uttryck i Swift

För att använda reguljära uttryck i Swift behöver vi importera "Foundation" biblioteket. Sedan kan vi använder oss av metoder från "NSRegularExpression" klassen för att skapa och utföra vår sökning eller manipulation. 

```Swift
import Foundation 

let text = "Detta är en text med siffror 12345 och symboler !@#"

do {
	// Skapar ett reguljärt uttryck för att hitta siffror
	let regex = try NSRegularExpression(pattern: "[0-9]+")
	
	// Utför sökningen på vår text
	let matches = regex.matches(in: text, range: NSRange(text.startIndex..., in: text))
	
	// Skriver ut matchningarna
	for match in matches {
		print(text[Range(match.range, in: text)!])
	}
} catch {
	print("Ogiltigt reguljärt uttryck: \(error)")
}
```

Kodblocket ovan skriver ut följande:

```
12345
```

## Djupare utforskning av reguljära uttryck

Reguljära uttryck kan användas på olika sätt för att söka, matcha och manipulera text. Vi kan till exempel använda speciella symboler för att matcha specifika mönster eller använda modifierare för att göra sökningen mer flexibel. Det finns också flera onlineverktyg där man kan testa och experimentera med sina reguljära uttryck, som [RegExr](https://regexr.com/) eller [Regex101](https://regex101.com/).

## Se även

- [Apple - NSRegularExpression](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [Swift by Sundell - Regular Expressions](https://www.swiftbysundell.com/tips/regular-expressions-in-swift/)