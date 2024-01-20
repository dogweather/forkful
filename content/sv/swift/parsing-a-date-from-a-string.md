---
title:                "Analysera ett datum från en sträng"
html_title:           "Kotlin: Analysera ett datum från en sträng"
simple_title:         "Analysera ett datum från en sträng"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Vad & varför?
Att tolka ett datum från en sträng innebär att konvertera en textrepresentation av ett datum till ett format som datorn kan förstå och behandla. Programmerare gör detta för att läsa datumbaserad information från källor som textfiler eller användarinmatning. 

## Så här gör du:
För att göra detta i Swift använder du `DateFormatter` -klassen. Här är ett exempel:

```swift
let datumSträng = "2022-07-14"
let formatter = DateFormatter()

formatter.dateFormat = "yyyy-MM-dd"
if let datum = formatter.date(from: datumSträng) {
    print("Datum: \(datum)")
} else {
    print("Fel vid tolkning av datum")
}
```

När du kör detta kodexempel skrivs "Datum: 2022-07-14 00:00:00 +0000" ut i konsolen.

## Fördjupning
Historiskt sett har datumtolkning varit en krånglig process på grund av olika datumformat runt om i världen. Swift`DateFormatter` gör det enkelt genom att tillåta att man ställer in önskat format för att likna den sträng du försöker tolka. 

Alternativ till Swift 'DateFormatter' inkluderar manual konvertering (som innebär att parsa strängen tecken för tecken), men det leder ofta till mer komplicerad och felbenägen kod. 

Genom att använda Swifts inbyggda `DateFormatter`, kan du lita på att datumen tolkas korrekt, förutsatt att du har angivit rätt datumformat. Det tar även hand om tidzoner, vilket kan vara en huvudvärk att hantera manuellt.

## Se även
- [Apple Developer Documentation: DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)
- [Apple's Swift book: Dates and Times](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/DataFormatting/Articles/dfDateFormatting10_4.html#//apple_ref/doc/uid/TP40002369-SW1)
- [StackOverflow: DateFrom string in Swift](https://stackoverflow.com/questions/35700281/date-format-in-swift)