---
title:                "Store bokstaver i en streng"
html_title:           "Kotlin: Store bokstaver i en streng"
simple_title:         "Store bokstaver i en streng"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Når vi kapitaliserer en streng, endrer vi første bokstav i hvert ord til en stor bokstav. Dette gjør det lettere å lese teksten og gir en mer strukturert presentasjon. Programmører gjør dette for å øke lesbarheten og gjøre koden deres mer konsistent.
## Hvordan gjør man det:
```Kotlin
fun capitalizeString(input: String): String {
    val words = input.split(" ")
    val capitalizedWords = ArrayList<String>()
    for (word in words) {
        capitalizedWords.add(word.capitalize())
    }
    return capitalizedWords.joinToString(" ")
}

fun main() {
    val string = "dette er en setning som skal bli kapitalisert"
    println(capitalizeString(string))
}
```
Output:
```
Dette Er En Setning Som Skal Bli Kapitalisert
```
## Dypdykk
Kapitalisering av strenger er en vanlig praksis i ulike programmeringsspråk, inkludert Kotlin. Det gir en tydeligere presentasjon av teksten og kan også hjelpe til med å skille mellom variable og funksjoner. Det finnes også alternative måter å kapitalisere en streng på, for eksempel å endre bokstaven til stor bokstav ved hjelp av indeksering. Implementeringen av kapitalisering i Kotlin er effektiv og kan også brukes på Unicode-tegn.
## Se også
[Official Kotlin Docs on String Manipulation](https://kotlinlang.org/docs/reference/basic-types.html#strings)

[Tutorialspoint on Capitalizing Strings in Kotlin](https://www.tutorialspoint.com/string-capitalize-function-in-kotlin)