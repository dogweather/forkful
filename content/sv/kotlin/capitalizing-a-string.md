---
title:                "Konvertera en sträng till versaler"
html_title:           "Kotlin: Konvertera en sträng till versaler"
simple_title:         "Konvertera en sträng till versaler"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför
Har du någonsin behövt ändra en sträng så att varje ord börjar med en stor bokstav? Det kan vara en ansträngande uppgift, men med Kotlin kan du enkelt utföra denna uppgift med hjälp av en inbyggd funktion.

## Hur man gör
```Kotlin
// Skapa en sträng
val sträng = "detta är en sträng som ska ändras"

// Använd funktionen "capitalize" för att ändra strängen
val nySträng = sträng.capitalize()

// Skriv ut den nya strängen
println(nySträng)

// Output: Detta är en sträng som ska ändras
```

## Deep Dive
Funktionen "capitalize" finns tillgänglig för alla strängar i Kotlin och används för att konvertera den första bokstaven i varje ord till en stor bokstav. Om strängen redan har en stor bokstav i början av ett ord, kommer den att förbli oförändrad. Om ett ord har en blandning av både små och stora bokstäver i början, kommer endast den första bokstaven att ändras till en stor bokstav.

En annan användbar funktion för att ändra storlek på bokstäver är "decapitalize". Denna funktion gör motsatsen och omvandlar den första bokstaven i varje ord till en liten bokstav.

Du kan också ange en parametrar för att specificera vilka tecken som ska vara med i konverteringen. Till exempel kan du bara ändra storleken på bokstäverna i ett specifikt intervall i en sträng.

## Se även
- [Officiell Kotlin dokumentation för funktionen capitalize](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/capitalize.html)
- [Officiell Kotlin dokumentation för funktionen decapitalize](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/decapitalize.html)
- [Mer om grundläggande strängoperationer i Kotlin](https://kotlinlang.org/docs/reference/basic-types.html#strings)