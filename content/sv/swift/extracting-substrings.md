---
title:                "Extrahera delsträngar"
html_title:           "Arduino: Extrahera delsträngar"
simple_title:         "Extrahera delsträngar"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

# Extrahera delsträngar med Swift

## Vad & Varför?
Att extrahera delsträngar innebär att ta en mindre del av en större sträng. Programmerare gör det när de behöver behandla specifika delar av data inom strängarna, såsom användarnamn i e-postadresser eller efternamn i fullständiga namn.

## Hur man gör:
Här är hur du kan extrahera delsträngar i Swift med hjälp av ranges och String.Index-typen:

```Swift
let text = "Hej, världen!"
let start = text.index(text.startIndex, offsetBy: 5)
let end = text.index(text.startIndex, offsetBy: 10)
let substring = text[start..<end]
print(substring) // utskrift: "värld"
```

Output:
```
värld
```
## Djupdykning
Extraktion av delsträngar kommer från tidiga dagar av programmering när ren data oftast var oorganiserade teckenströmmar. I Swift kan du även använda `.prefix()` och `.suffix()` metoder för att extrahera delsträngar från början eller slutet av en sträng. Metoden ovan använder specifika index, vilket ger mer kontroll över vilken del av strängen som extraheras.

## Se Även
För mer detaljerade tekniker och strategier för substräng extraktion i Swift, besök:

1. Swift Docs: [Arbeta med strängar](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)