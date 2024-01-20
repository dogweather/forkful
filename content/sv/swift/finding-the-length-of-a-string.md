---
title:                "Hitta längden på en sträng"
html_title:           "Arduino: Hitta längden på en sträng"
simple_title:         "Hitta längden på en sträng"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att hitta längden på en sträng är att räkna antalet tecken i den. Programmers gör detta för att förstå data de behandlar, som att se om användarinmatningar uppfyller längdkrav.

## Hur man:

För att hitta längden på en sträng i Swift, använder du egenskapen `count` på din sträng. Här är exempel:

```Swift
let str = "Hej, Världen!"
print("Strängens längd är: \(str.count)")
```

Detta kodexempel skriver ut:

```Swift
Strängens längd är: 14
```

Så enkelt är det! Swift räknar med Unicode-skalbara skalärer, vilket fungerar för de flesta internationella språk, inte bara standard engelska tecken.

## Djupdykning 

För att effektivt räkna längden på en sträng, behandlar Swift strängen som en samling av Unicode-skalbara skalärer istället för individa bytes. Eftersom vissa tecken kan representeras av mer än en byte, räknar `count` egenskapen korrekt antalet tecken istället för bytes.

Ett alternativ till användning av `count` egenskapen är `utf8.count`, `utf16.count` eller `unicodeScalars.count`. Dessa metoder returnerar inte nödvändigtvis samma resultat som `count` när det gäller internationella tecken som kan kodas i mer än en byte.

Men, för de flesta ändamål, kommer `count` att ge dig den information du behöver på ett enkelt och lättförståeligt sätt.

## Se också

För mer information om att hantera strängar i Swift, utforska följande länkar:

- [Swift Dokumentation: Strängar och tecken](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Swift av Sundell: Arbeta med strängar i Swift](https://www.swiftbysundell.com/basics/strings/)