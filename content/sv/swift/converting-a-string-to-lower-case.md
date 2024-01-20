---
title:                "Omvandla en sträng till gemener"
html_title:           "Arduino: Omvandla en sträng till gemener"
simple_title:         "Omvandla en sträng till gemener"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

### Vad & Varför?
Att konvertera en sträng till gemener innebär att ändra alla bokstäver i strängen till små bokstäver. Programmerare gör detta för att standardisera strängdata och för att förenkla jämförelser och sökningar.

### Hur man gör: 
För att konvertera en sträng till gemener i Swift använder vi metoden `lowercased()`. 

```Swift
let myString = "Hello, World!"
let lowerCasedString = myString.lowercased()
print(lowerCasedString) // "hello, world!"
```
I exemplet ovan konverterar vi strängen "Hello, World!" till gemener och skriver sedan ut resultatet.

### Fördjupning
Historiskt sett har konvertering av strängar till gemener i programmeringsspråk varit användbart för att hantera skillnader i skiftläget för bokstäver. Det finns alternativ till metoderna för konvertering till gemener, beroende på vilka språk du arbetar med.

I Swift är en alternativ metod att använda `caseInsensitiveCompare(_:)` vid jämförelse, vilket jämför två strängar utan att skilja på versaler och gemener.

```Swift
let myString1 = "Hello, World!"
let myString2 = "hello, world!"
let same = myString1.caseInsensitiveCompare(myString2) == .orderedSame
print(same) // Prints "true"
```

### Se även: 
För relaterade ämnen och läsning kan du kolla följande länkar: 
3. Stack Overflow diskussion om att använda `lowercased()` - [länk här](https://stackoverflow.com/questions/26322681/should-i-use-string-lowercase-string-or-string-caseinsensitivecompare-in-swift)