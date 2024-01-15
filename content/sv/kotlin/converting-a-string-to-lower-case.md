---
title:                "Omvandla en sträng till gemener"
html_title:           "Kotlin: Omvandla en sträng till gemener"
simple_title:         "Omvandla en sträng till gemener"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför

Att konvertera en sträng till gemener (lower case) kan vara användbart för att jämföra strängar på ett icke-ordinärt sätt eller för att anpassa data för olika formateringar, som till exempel att omvandla en sträng till små bokstäver för att visa en enhetlig presentation.

## Hur man gör det

För att konvertera en sträng till gemener i Kotlin, använd funktionen `toLowerCase()` på strängen. Här är ett enkelt exempel:

```Kotlin
val str = "Hej, Världen!"
println(str.toLowerCase()) // "hej, världen!"
```

I det här fallet är `toLowerCase()` en inbyggd funktion i Kotlin, så du behöver inte importera något extra bibliotek för att använda den.

## Djupdykning

När du använder `toLowerCase()` i Kotlin, tar den hänsyn till språk och regioninställningar för att säkerställa att konverteringen sker på ett korrekt sätt. Det är också viktigt att notera att `toLowerCase()` endast konverterar bokstäver i det engelska alfabetet till gemener, så bokstäver som "Å", "Ä" och "Ö" kommer inte att konverteras. För att hantera specialtecken och icke-engelska bokstäver, kan du använda funktionen `Locale` för att ange en specifik språk- och regionkod.

```Kotlin
val str = "Hej, Världen!"
println(str.toLowerCase(Locale("sv","SE"))) // "hej, världen!"
```

Om du vill ha mer finjusterad kontroll över vilka tecken som ska konverteras, kan du använda `map` tillsammans med en benämnare för unicode ( \\u för Kotlin) för att ange specifika tecken att konvertera.

```Kotlin
val str = "Hej, Världen!"
val lowerCaseMapping = mapOf("\\u0041" to "\\u0061") // Konverterar bokstaven "A" till "a"

println(str.map { lowerCaseMapping[it].toString() }.joinToString("")) // "hej, Världen!"
```

## Se även

- [Kotlin dokumentation för String Class](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- [Language & Region codes for Unicode](https://www.unicode.org/cldr/charts/latest/supplemental/territory_language_information.html)
- [Convert string to lowercase in Java](https://www.geeksforgeeks.org/convert-string-lowercase-java/)