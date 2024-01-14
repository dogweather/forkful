---
title:    "Kotlin: Att hitta längden på en sträng."
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför
Att kunna hitta längden på en sträng är en grundläggande del av många programmeringsuppgifter. Det är också ett verktyg som används i många olika områden, oavsett vilket programmeringsspråk man jobbar med.

## Hur man gör det
För att hitta längden på en sträng i Kotlin, kan vi använda metoden "length" som finns tillgänglig för alla strängar. Här är ett exempel på hur det kan se ut:

```Kotlin
val str = "Hej alla!"
println(str.length)
```

Output: 9

Detta kodexempel skapar en variabel som innehåller en sträng och sedan använder vi "length" metoden för att hitta längden på den. Resultatet skrivs sedan ut på skärmen.

En annan metod för att hitta längden på en sträng i Kotlin är att använda "count" funktionen. Här är ett exempel på hur det kan se ut:

```Kotlin
val str = "Hej alla!"
println(str.count())
```

Output: 9

Båda metoderna ger samma resultat, så det är upp till dig vilken du föredrar att använda.

## Djupdykning
För att förstå hur längden på en sträng faktiskt hittas, behöver vi veta att en sträng är en sekvens av tecken. Därför räknas längden på en sträng genom att räkna antalet tecken i strängen.

Hur gör man då detta? Först och främst så förser Kotlin oss med metoden "length" eller "count" som vi nämnde tidigare. Men faktum är att båda dessa metoder använder sig av en annan funktion som heter "size", som faktiskt är den som räknar tecknen i strängen.

"Size" funktionen är en standardfunktion i Kotlin och är tillgänglig för alla typer av collection-objekt, inklusive strängar. Den fungerar genom att loopa igenom alla tecken i strängen och öka antalet med ett varje gång. Detta sätt att räkna längden på en sträng är både snabbare och mer exakt jämfört med att räkna antalet tecken en och en.

## Se även
- [Kotlin referens för strängar](https://kotlinlang.org/docs/reference/strings.html)
- [Official Kotlin Tutorial](https://kotlinlang.org/docs/tutorials/)
- [Kotlin on GitHub](https://github.com/JetBrains/kotlin)