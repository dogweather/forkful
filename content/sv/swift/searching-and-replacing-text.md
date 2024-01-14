---
title:                "Swift: Sökning och ersättning av text"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Varför

Att söka och ersätta text är en viktig del av Swift-programmering eftersom det gör det enkelt att göra massiva ändringar i din kod på en gång. Det sparar tid och minskar risken för misstag, så det är definitivt värt att lära sig.

# Så här

Att söka och ersätta text är enkelt med hjälp av Swift-kod. Använd "replacingOccurrences" -funktionen för att ange den text du vill ersätta och vad du vill ersätta den med. Här är ett exempel:

```Swift
let originalText = "Hej, jag gillar att koda Swift."
let newText = originalText.replacingOccurrences(of: "Swift", with: "Java")
print(newText)
```

Konsolresultatet blir "Hej, jag gillar att koda Java."

Du kan också använda "replacingCharacters" -funktionen för att ersätta specifika tecken i en sträng med andra tecken. Här är ett exempel:

```Swift
let originalText = "Swift är bäst!"
let newText = originalText.replacingCharacters(in: 1...3, with: "PROGRAMMERING")
print(newText)
```

Konsolresultatet blir "SPROGRAMMERINGt är bäst!"

# Djupdykning

När du söker och ersätter text i Swift är det viktigt att förstå hur funktionerna fungerar. "replacingOccurrences" -funktionen söker efter en viss sekvens av tecken och ersätter det med ett annat värde, medan "replacingCharacters" -funktionen ersätter tecken i en specifik del av strängen baserat på det angivna intervallet.

En annan viktig del av att söka och ersätta är användningen av "case sensitive" -parametern. Om du vill att sökningen ska vara "case insensitive" kan du ange "options: .caseInsensitive". På så sätt kommer både stora och små bokstäver att behandlas lika.

# Se även

- Lär dig mer om Swift-kodkonventioner: https://swift.org/documentation/api-design-guidelines/
- Utforska fler funktioner för att manipulera text i Swift: https://developer.apple.com/documentation/swift/string
- Hitta mer avancerade sök- och ersättningsmetoder i Swift: https://www.hackingwithswift.com/quick-start/swiftui/advanced-searching-using-swiftui