---
title:                "Att göra en sträng versal"
html_title:           "Bash: Att göra en sträng versal"
simple_title:         "Att göra en sträng versal"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att kapitalisera en sträng innebär att göra varje bokstav i strängen till en stor bokstav. Programmerare gör detta för att standardisera textdata, förbättra läsbarheten eller för estetiska skäl, till exempel att börja varje ord i en rubrik med stor bokstav.

## Hur man gör:
Swift har inbyggda metoder för att jobba med text. Där ingår `uppercased()` för att göra alla bokstäver stora och `capitalized` för att storstil varje ord. Här är några exempel:

```Swift
let smallText = "jag älskar programmering."
let uppercasedText = smallText.uppercased() // "JAG ÄLSKAR PROGRAMMERING."
print(uppercasedText)

let title = "detta är en titel"
let capitalizedTitle = title.capitalized // "Detta Är En Titel"
print(capitalizedTitle)
```

Sample output:

```
JAG ÄLSKAR PROGRAMMERING.
Detta Är En Titel
```

## Fördjupning
Att ändra storlek på bokstäver har varit viktigt ända sedan tryckpressens dagar. Det gör text enhetlig och förbättrar läsbarheten. `uppercased()` och `capitalized` är Swifts verktyg för detta. Historiskt har andra språk som Python och Java liknande metoder, som `.upper()` eller `.toUpperCase()`.

Swifts `capitalized` är smart. Den kapitaliserar inte bara första bokstaven i ett ord, utan tar hänsyn till punktuering och andra skiljetecken. Det är bra för korrekt formattering av meningar. Men vara medveten, den hanterar inte lokala regler, som speciella regler för svenska språket.

Det finns alternativ som `localizedUppercaseString` i Swift som kan hantera lokala regler bättre, om det behövs för specifika användningsfall.

## Se även
- Swifts officiella dokumentation om Strings: [https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- En guide till Swifts `String` API: [https://www.hackingwithswift.com/articles/141/8-swift-string-functions](https://www.hackingwithswift.com/articles/141/8-swift-string-functions)
