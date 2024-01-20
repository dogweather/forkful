---
title:                "Omvandla en sträng till gemener"
html_title:           "Arduino: Omvandla en sträng till gemener"
simple_title:         "Omvandla en sträng till gemener"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att konvertera en sträng till gemener innebär att ändra alla tecken i en textsträng till små bokstäver. Detta görs ofta när programmerare behöver jämföra strängar på ett sätt som inte är känsligt för användning av stora och små bokstäver.

## Hur man gör:
Här är ett exempel på hur du konverterar en sträng till gemener i Kotlin:

```Kotlin
fun main() {
    val original = "Hej Världen!"
    val toLowercase = original.toLowerCase()

    println(toLowercase)
}
```

När du kör den här koden, kommer utskriften att vara:

```Kotlin
"hej världen!"
```

## Djupdykning:
Historiskt sett har konvertering till gemener varit ett grundläggande verktyg inom textbehandling, särskilt i relation till sökning och jämförelse. Med detta kan programmerare jämföra strängar oberoende av hur de skrivs.

Att använda `toLowerCase()`-funktionen är den mest direkta metoden för att konvertera en sträng till gemener i Kotlin, men det finns alternativ som kan användas i mer specifika sammanhang. Till exempel, om du arbetar i en situation där teckenuppsättningen inte är fastställd, kan du använda `String.format()`:

```Kotlin
val original = "Hej Världen!"
val toLowercase = String.format("%s", original).toLowerCase()

println(toLowercase)
```

`toLowerCase()`-funktionen använder plattformens standard-locale för att konvertera tecknen. För att specificera en annan locale, kan argument såsom `Locale.US` passeras in:

```Kotlin
val toLowercaseWithLocale = original.toLowerCase(Locale.US)

println(toLowercaseWithLocale)
```

## Se även:
Du kan även kolla in dessa referenser för mer information:
1. Kotlin's officiella dokumentation om `toLowerCase()` funktion: [https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/to-lower-case.html](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/to-lower-case.html)