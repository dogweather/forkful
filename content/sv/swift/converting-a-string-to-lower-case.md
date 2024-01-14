---
title:    "Swift: Omvandling av en sträng till gemener"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Varför
I den här bloggposten kommer vi att titta på hur man konverterar en sträng till små bokstäver i Swift. Det är ett vanligt problem som ofta uppstår när man hanterar inmatning från användare eller jämför strängar i en applikation. Genom att konvertera en sträng till små bokstäver kan du enkelt uppnå enhetlighet och korrekta jämförelser.

# Hur man gör
För att konvertera en sträng till små bokstäver kan du använda metoden `.lowercased()` på din sträng. Här är ett exempel:

```Swift
let originalString = "SWIFT ÄR KUL!"
let lowercasedString = originalString.lowercased()
print(lowercasedString)
```

Output:
```
swift är kul!
```

# Djupdykning
När du använder `.lowercased()`-metoden i Swift kommer alla bokstäver i strängen att konverteras till små bokstäver enligt Unicode-standarder. Det betyder att även bokstäver från andra språk än engelska kommer att konverteras till små bokstäver enligt deras Unicode-ekvivalenter.

Om du vill begränsa konverteringen endast till engelska bokstäver kan du använda `.lowercased(with: Locale)`-metoden och specificera önskad lokal. Till exempel:

```Swift
let originalString = "SWIFT ÄR KUL!"
let lowercasedString = originalString.lowercased(with: Locale(identifier: "en"))
print(lowercasedString)
```

Output:
```
swift är kul!
```

Det är också värt att nämna att `.lowercased()`-metoden inte ändrar originalsträngen, utan skapar en ny sträng med de konverterade bokstäverna.

# Se även
- [Dokumentation om `.lowercased()`-metoden](https://developer.apple.com/documentation/swift/string/1783473-lowercased)
- [Unicode-språkkoder](https://www.unicode.org/cldr/charts/latest/supplemental/language_territory_information.html)