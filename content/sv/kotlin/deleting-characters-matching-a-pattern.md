---
title:    "Kotlin: Radera tecken som matchar ett mönster"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Varför

Att ta bort tecken som matchar ett mönster kan vara en användbar teknik inom programmering för att filtrera och manipulera data på ett effektivt sätt.

## Hur man gör det

För att ta bort tecken som matchar ett visst mönster i Kotlin, kan vi använda en funktion som heter `replace(regex: Regex, replacement: String)`. Denna funktion tar emot två parametrar - ett reguljärt uttryck (Regex) och en sträng som ska ersätta de matchande tecknen.

```Kotlin
val string = "Välkommen till Kotlin bloggen!"
val regex = Regex("[a-zA-Z]") // Regular expression för bokstäver a till z, både stora och små tecken
val newString = string.replace(regex, "") // Tar bort alla bokstäver från strängen
println(newString) // Output: "!"

```

I det här exemplet tar vi först en given sträng och definierar sedan ett reguljärt uttryck som matchar alla bokstäver från a till z, både stora och små. Sedan använder vi `replace()` funktionen för att ersätta varje matchande tecken med en tom sträng, vilket resulterar i att alla bokstäver tas bort från den ursprungliga strängen.

Vi kan också använda reguljära uttryck för att matcha och ta bort specifika tecken eller kombinationer av tecken. Till exempel, om vi vill ta bort alla siffror från en sträng, kan vi använda `[0-9]` i vårt reguljära uttryck.

```Kotlin
val string = "123abc456def789"
val regex = Regex("[0-9]")
val newString = string.replace(regex, "")
println(newString) // Output: "abcdef"

```

Vi kan också använda `^` för att negerealera ett reguljärt uttryck, vilket betyder att vi kan ta bort alla tecken som inte matchar vårt uttryck.

```Kotlin
val string = "Welcome to 123 Kotlin 456 blog!"
val regex = Regex("[^a-zA-Z ]") // Tar bort alla tecken som inte är bokstäver eller mellanslag
val newString = string.replace(regex, "")
println(newString) // Output: "Welcome to Kotlin blog"

```

## Djupdykning

Att ta bort tecken som matchar ett visst mönster kan vara användbart när vi till exempel behöver rensa en sträng från specifika tecken innan vi utför ytterligare manipulationer på den. Det kan också vara användbart för att filtrera och bearbeta stora datamängder där vi vill ta bort oönskade tecken eller kombinationer av tecken.

Men det är viktigt att vara försiktig när vi använder reguljära uttryck, eftersom ett felaktigt uttryck kan resultera i att fel tecken tas bort eller inte tas bort alls. Det är också viktigt att ha i åtanke att reguljära uttryck kan variera mellan olika programmeringsspråk och implementeringar.

## Se även

- [Kotlin Regex documentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)

- [Regular Expression Tutorial](https://regexone.com/) (engelska)