---
title:    "Kotlin: Stora bokstäver i en sträng"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Varför

Att kunna konvertera en sträng till stora bokstäver kan vara användbart i många situationer. Det kan hjälpa till att förbättra läsbarheten av texten eller användas för att jämföra strängar.

## Hur man gör

Det finns olika sätt att få en sträng att börja med stora bokstäver. En av de enklaste metoderna är att använda inbyggda funktionen ".capitalize()". Denna funktion kommer att returnera en kopia av strängen med första bokstaven stor, och resten av bokstäverna oförändrade som visas nedan:

```Kotlin
val name = "swedish"
println(name.capitalize())
// Output: Swedish
```

En annan metod är att använda ".toUpperCase()" som kommer att konvertera hela strängen till stora bokstäver:

```Kotlin
val country = "sWeDeN"
println(country.toUpperCase())
// Output: SWEDEN
```

Det finns också möjlighet att använda en anpassad funktion för att konvertera en sträng till stora bokstäver, som visas nedan:

```Kotlin
fun capitalizeString(str: String): String{
    var result = ""
    for (char in str){
        result += char.toUpperCase()
    }
    return result
}

val text = "hello world"
println(capitalizeString(text))
// Output: HELLO WORLD
```

## Deep Dive

Det finns flera saker att tänka på när man konverterar en sträng till stora bokstäver. En sak är att det finns skillnader i beteendet hos vissa bokstäver beroende på språket. Till exempel, i det svenska alfabetet är "å", "ä" och "ö" bokstäverna efter "z", medan i det engelska alfabetet är de bokstäverna efter "a". Detta kan leda till olika utmaningar när man hanterar strängar på olika språk.

En annan sak att tänka på är att konvertera en sträng till stora bokstäver kan påverka prestandan om det görs på många olika strängar i en loop. Att använda inbyggda funktioner som ".capitalize()" är oftast mer effektivt än att skriva en egen funktion.

Det finns också möjlighet att konvertera en sträng till små bokstäver med ".toLowerCase()", vilket kan vara användbart för att jämföra strängar oberoende av storlek på bokstäver.

## Se även

- *Kotlin Standard Library: Strings* (https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- *Kotlin String Functions* (https://www.tutorialkart.com/kotlin/kotlin-string-functions/)