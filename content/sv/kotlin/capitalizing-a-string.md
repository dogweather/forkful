---
title:    "Kotlin: Att göra en sträng stor bokstav"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att skriva kod handlar inte bara om att skapa fungerande program, det handlar också om att göra koden lätt att läsa och förstå. Att kapitalisera en sträng kan vara ett sätt att förbättra läsbarheten och göra koden mer lättförståelig.

## Hur man gör

Att kapitalisera en sträng i Kotlin är en enkel process som kan utföras på flera olika sätt. Ett sätt är att använda inbyggda funktionen "capitalize()". Till exempel:

```Kotlin
val str = "hej alla"
val capitalizedStr = str.capitalize()

println(capitalizedStr)
// Output: "Hej alla"
```

En annan möjlighet är att använda en for-loop för att iterera över varje bokstav i strängen och omvandla den första bokstaven till en stor bokstav. Till exempel:

```Kotlin
val str = "hej alla"
val chars = str.toCharArray()

for (i in chars.indices) {
    if (i == 0) {
        chars[i] = chars[i].toUpperCase()
    }
}

val capitalizedStr = String(chars)

println(capitalizedStr)
// Output: "Hej alla"
```

Det finns också möjlighet att använda en förlängningsfunktion, som är en funktion som utökar funktionaliteten för en befintlig typ. Till exempel:

```Kotlin
fun String.capitalizeFirstLetter(): String {
    if (isNotEmpty()) {
        return substring(0, 1).toUpperCase() + substring(1)
    }
    return this
}

val str = "hej alla"
val capitalizedStr = str.capitalizeFirstLetter()

println(capitalizedStr)
// Output: "Hej alla"
```

## Fördjupning

Det finns flera saker att tänka på när man kapitaliserar en sträng. En viktig aspekt är att koden ska vara så effektiv som möjligt, särskilt om man behöver kapitalisera stora mängder data. Det är också viktigt att ta hänsyn till internationella tecken och brytpunkter för korrekt kapitalisering i olika språk.

En annan viktig aspekt är hantering av specialfall, som till exempel förkortningar som behöver kapitaliseras på ett visst sätt. Det är också viktigt att se till att den ursprungliga strängen inte ändras av misstag, utan endast den kapitaliserade versionen av strängen.

## Se även

- [Kotlin String API reference](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- [GeeksforGeeks tutorial on string manipulation in Kotlin](https://www.geeksforgeeks.org/string-manipulation-in-kotlin/)
- [Official Kotlin documentation on extension functions](https://kotlinlang.org/docs/reference/extensions.html)