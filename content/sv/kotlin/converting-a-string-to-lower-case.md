---
title:                "Kotlin: Omvandla en sträng till små bokstäver"
simple_title:         "Omvandla en sträng till små bokstäver"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför

Att konvertera en sträng till gemener är en vanlig och användbar funktion inom programmering, speciellt i situationer där man behöver jämföra strängar eller söka igenom dem. Det finns flera olika metoder för att utföra detta, men i denna bloggpost kommer vi att fokusera på hur man kan göra det i Kotlin.

## Så här gör du

För att konvertera en sträng till gemener i Kotlin, kan du använda inbyggda funktionen `toLowerCase()` på en variabel innehållande en sträng. Här är ett exempel på hur koden kan se ut:

```Kotlin
val originalStrang = "HeLlO WoRlD"
val nyStrang = originalStrang.toLowerCase()
println(nyStrang) //output: hello world
```

Som du kan se, så konverterar `toLowerCase()` alla bokstäver i strängen till gemener. Detta gör jämförelser och sökningar mellan strängar mer exakta, eftersom stora och små bokstäver inte längre spelar någon roll.

## Djupdykning

Förutom att använda den inbyggda funktionen `toLowerCase()`, finns det även andra sätt att åstadkomma samma sak i Kotlin. En annan metod är att använda `Regex` klassen för att matcha och byta ut bokstäver med hjälp av metoden `replaceAll()`. Detta kan vara en användbar teknik om du vill göra mer komplexa sökningar och ersättningar i din sträng.

Det är också viktigt att notera att konverteringen till gemener sker enligt Unicode-standarden, vilket innebär att alla specialtecken och accenter kommer att behålla sin ursprungliga form även efter konverteringen.

## Se även

- [Kotlin Docs: toLowerCase()](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/to-lower-case.html)
- [Kotlin Docs: Regex](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)
- [JavaDocs: Character.toLowerCase()](https://docs.oracle.com/javase/8/docs/api/java/lang/Character.html#toLowerCase-char-)