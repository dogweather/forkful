---
title:                "Omvandla en sträng till små bokstäver"
html_title:           "Kotlin: Omvandla en sträng till små bokstäver"
simple_title:         "Omvandla en sträng till små bokstäver"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Vad & Varför?
När vi pratar om att konvertera en sträng till gemener i Kotlin, menar vi att göra alla bokstäver i strängen små. Detta är en vanlig operation inom programmering och används främst för att förbättra sök- eller jämförelsefunktioner.

## Hur gör man:
```
// Enkelt exempel på konvertering av en sträng till gemener
val str = "HEJ!"
val lowercaseStr = str.toLowerCase()
println(lowercaseStr)

// Output: hej!
```

```
// Användardefinierad funktion för konvertering av sträng till gemener
fun toLowerCase(str: String): String {
    val sb = StringBuilder()
    
    for (c in str) {
        if (c in 'A'..'Z') { // Kontrollerar om tecknet är en stor bokstav
            sb.append(c.toLowerCase()) // Konverterar till gemener
        } else {
            sb.append(c)
        }
    }
    
    return sb.toString()
}

// Användning av den användardefinierade funktionen
val str = "HELLO WORLD!"
val lowercaseStr = toLowerCase(str)
println(lowercaseStr)

// Output: hello world!
```

## Djupdykning:
Historiskt sett, har konvertering av strängar till gemener använts för att underlätta sortering och jämförelse av textdata. Det finns även andra alternativ för att åstadkomma samma resultat, som t.ex. att använda inbyggda jämförelsefunktioner som inte är fallkänsliga.

När det kommer till implementationen av att konvertera en sträng till gemener, finns det flera olika metoder och algoritmer som kan användas. Valet av metod kan bero på faktorer som prestanda och minnesanvändning.

## Se även:
- [Kotlin: Strings and Characters](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Java String: toLowerCase()](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toLowerCase--)