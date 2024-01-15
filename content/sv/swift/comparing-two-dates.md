---
title:                "Jämförelse av två datum"
html_title:           "Swift: Jämförelse av två datum"
simple_title:         "Jämförelse av två datum"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför

Att jämföra två datum är en vanlig uppgift inom programmering och är användbart för att utföra specifika åtgärder baserat på huruvida ett datum är före eller efter ett annat.

## Hur man gör det

```Swift
// Skapa två Datum-objekt att jämföra
let förstaDatum = Date()
let andraDatum = Date()

// Jämför de två datumen
if förstaDatum < andraDatum {
    print("Första datumet är tidigare än det andra datumet.")
} else if förstaDatum > andraDatum {
    print("Första datumet är senare än det andra datumet.")
} else {
    print("Båda datumen är samma.")
}

// Utmatning: Första datumet är tidigare än det andra datumet.
```

Genom att använda jämförelseoperatorer som "<" och ">" kan vi enkelt jämföra två Datum-objekt. Om det första datumet är tidigare än det andra, utför vi en specifik åtgärd. Om det andra datumet är tidigare eller om båda datumen är samma kan vi utföra andra åtgärder enligt behov.

## Djupdykning

Jämförelse av datum kan vara mer komplicerat än bara att jämföra två Datum-objekt. Ofta behöver vi ta hänsyn till tidszoner och olika kalendrar. För att göra detta finns det möjlighet att använda DateComponents och Calendar-klasserna, som ger mer precision och flexibilitet vid jämförelse av datum. Det är också viktigt att notera att jämförelser av datum kan påverkas av datorklockans inställningar, så det är viktigt att noga analysera och hantera denna aspekt för korrekta resultat.

## Se även

- [Apple Developer Documentation - Date](https://developer.apple.com/documentation/foundation/date)
- [Apple Developer Documentation - DateComponents](https://developer.apple.com/documentation/foundation/datecomponents)
- [Apple Developer Documentation - Calendar](https://developer.apple.com/documentation/foundation/calendar)