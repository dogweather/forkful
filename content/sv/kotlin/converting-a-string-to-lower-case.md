---
title:    "Kotlin: Omvandla en sträng till små bokstäver"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Varför

Att konvertera en sträng till små bokstäver kan vara användbart i många olika situationer. Det kan till exempel vara till hjälp när man vill jämföra textsträngar, eller när man vill göra en sökning på en sträng utan att skilja mellan stora och små bokstäver.

## Så här gör du

För att konvertera en sträng till små bokstäver kan du använda dig av `toLowerCase()` funktionen i Kotlin. Här är ett enkelt exempel:

```Kotlin
val sträng = "DEtTA Är EN StRÄNg"
val nySträng = sträng.toLowerCase()
println(nySträng)
```

Output:
```Kotlin
detta är en sträng
```

Som du kan se i exemplet så använde vi oss av `toLowerCase()` funktionen på vår ursprungliga sträng och tilldelade sedan resultatet till en ny variabel. Detta är bara ett enkelt exempel, men funktionen fungerar på samma sätt oavsett vilken sträng du använder den på.

## Djupdykning

I bakgrunden använder `toLowerCase()` funktionen sig av unicode-tecken för att konvertera strängen till små bokstäver. Detta innebär att även specialtecken och bokstäver från andra språk kommer att bli konverterade till små bokstäver.

Det är viktigt att notera att `toLowerCase()` funktionen inte ändrar på den ursprungliga strängen, utan returnerar en ny sträng med små bokstäver. Därför måste du spara resultatet i en variabel för att använda det senare i koden.

## Se också

Här är några relaterade länkar för att lära dig mer om Kotlin och strängar:

- [Kotlin Dokumentation: Basic Syntax](https://kotlinlang.org/docs/reference/basic-syntax.html)
- [GeeksForGeeks: Kotlin String toLowerCase() Methods](https://www.geeksforgeeks.org/kotlin-string-tolowercase-methods/)