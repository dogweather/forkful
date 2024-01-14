---
title:                "Kotlin: Hitta längden på en sträng"
simple_title:         "Hitta längden på en sträng"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att kunna hitta längden på en sträng är en vanlig åtgärd som utförs inom programmering. Det är en grundläggande färdighet som alla programmerare bör kunna, eftersom den är användbar i många olika situationer. I denna bloggpost kommer vi att titta närmare på varför det är viktigt att kunna hitta längden på en sträng, och hur man gör det med hjälp av Kotlin.

## Hur man gör det

För att hitta längden på en sträng i Kotlin kan man använda sig av funktionen "length". Den här funktionen används för att räkna antalet tecken i en sträng. Här nedan ser du ett exempel på hur man kan använda sig av funktionen "length" för att hitta längden på en sträng:

```Kotlin
val sträng = "Hejsan!"

println(sträng.length)    // Output: 7
```

Som du kan se i exemplet ovan så använder vi variabeln "sträng" för att lagra vår sträng. Sedan använder vi funktionen "length" för att hitta längden på strängen och skriver ut den med hjälp av "println" funktionen. Genom att köra koden kommer vi få ut värdet "7", vilket är antalet tecken i strängen "Hejsan!".

Man kan även använda sig av funktionen "length" för att hitta längden på en variabel som innehåller en sträng. Här nedan ser du ett annat exempel där vi skapar en variabel "namn" med värdet "Johan":

```Kotlin
val namn = "Johan"

println(namn.length)    // Output: 5
```

Som du kan se i exemplet ovan så kommer funktionen "length" att räkna antalet tecken i variabeln "namn", vilket i det här fallet är 5.

## Djupdykning

Nu när vi vet hur man använder funktionen "length" för att hitta längden på en sträng, låt oss titta på vad som faktiskt händer bakom kulisserna. När man anropar funktionen "length" på en sträng, så kommer den att returnera ett heltal som motsvarar antalet tecken i strängen. Detta innebär att om man skriver ut uttrycket "sträng.length" så kommer man att få ut ett heltal.

Det är viktigt att komma ihåg att funktionen "length" inte räknar med mellanslag eller andra tomma tecken. Den räknar enbart de faktiska tecknen i strängen.

## Se även

- [Kotlins dokumentation om strängar](https://kotlinlang.org/docs/basic-types.html#strings)
- [En interaktiv guide till Kotlin](https://play.kotlinlang.org/byExample/overview)
- [Kotlin kurs för nybörjare](https://www.udemy.com/course/kotlin-for-android-beginners/?referralCode=34881341C719119E43C2)