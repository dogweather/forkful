---
title:                "Hitta längden på en sträng"
html_title:           "Kotlin: Hitta längden på en sträng"
simple_title:         "Hitta längden på en sträng"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att hitta längden på en sträng är helt enkelt att bestämma hur många tecken som finns i en sträng. Detta är en vanlig operation inom programmering, eftersom det ofta är nödvändigt att hantera och behandla textdata.

## Så här gör du:
Kotlin har en inbyggd metod som heter `length`, som kan användas för att hitta längden på en sträng. Detta kan göras på olika sätt beroende på hur strängen är definierad.

```Kotlin
val string1 = "Välkommen!"
println(string1.length) // Output: 10

val string2 = "Hej"
println(string2.length) // Output: 3
```

## Djupdykning:
Att hitta längden på en sträng är en vanlig operation inom många programmeringsspråk. Detta görs ofta för att kontrollera gränser för inmatning av data eller för att behandla text på olika sätt.

En alternativ metod för att hitta längden på en sträng är att använda `StringTokenizer` i Java, men Kotlin gör det enklare genom sin inbyggda `length` metod.

Det är också viktigt att notera att längden på en sträng beräknas baserat på antalet tecken och inte antalet ord.

## Se även:
- [Kotlin Standard Library - Strings](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- [String length vs. array size in Java](https://stackoverflow.com/questions/2665550/string-length-vs-array-size-in-java)