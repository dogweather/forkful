---
title:                "Konvertering av en streng til små bokstaver"
html_title:           "Kotlin: Konvertering av en streng til små bokstaver"
simple_title:         "Konvertering av en streng til små bokstaver"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor
Så du lurer kanskje på hvorfor noen ville ønske å konvertere en streng til små bokstaver? Vel, det kan være flere grunner til det. Å konvertere en streng til små bokstaver kan gjøre det enklere å håndtere og sammenligne tekst, spesielt når du jobber med brukerinndata eller databasedata som kan variere i hoved- og små bokstaver.

## Hvordan gjøre det
La oss se på et eksempel på hvordan du kan konvertere en streng til små bokstaver i Kotlin:

```
fun main() {
    val string = "Hei, dette er en Streng!"
    println(string.toLowerCase())
}
```

**Output:**
> hei, dette er en streng!

For å konvertere en streng til små bokstaver kan du bruke Kotlin's `toLowerCase()` metode, som returnerer en ny streng med små bokstaver. Dette er en ikke-destruktiv operasjon, noe som betyr at originalstrengen blir uendret.

En annen måte å konvertere en streng til små bokstaver er å bruke `operator` nøkkelordet og `String`'s `plus()` metode. Her er et eksempel på hvordan det kan gjøres:

```
fun main() {
    val string = "Hei, dette er en Streng!"
    val newString = string + string
    println(string.toLowerCase())
    println(newString.toLowerCase())
}
```

**Output:**
> hei, dette er en streng!
> hei, dette er en streng!hei, dette er en streng!

Som du kan se, returnerer `toLowerCase()` metoden en ny streng, mens `plus()` operatoren endrer den eksisterende strengen og legger til en ny streng med små bokstaver.

## Dypdykk
Nå som du vet hvordan du kan konvertere en streng til små bokstaver, la oss se på noen ting du bør være oppmerksom på når du jobber med dette i Kotlin.

**NullPointerException (NPE)**
Hvis du prøver å bruke `toLowerCase()` metoden på en `null`verdi, vil du få en NPE. For å unngå dette, må du først sjekke om strengen er `null` før du konverterer den til små bokstaver.

**Foreground og background tråder**
Kotlin's `toLowerCase()` metode utfører operasjonen på hovedtråden, noe som kan føre til ytelsesproblemer hvis strengen er lang og behandlingen tar lang tid. For å unngå dette, kan du bruke `toLowerCase(Locale.getDefault())` metoden, som vil utføre konverteringen i en bakgrunnstråd. Dette vil forbedre ytelsen og sikre at appen din ikke fryser mens den konverterer strengen.

## Se også
- [Kotlin's Strings og Characters dokumentasjon](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Hvordan sammenligne små og store bokstaver i Kotlin](https://www.programiz.com/kotlin-programming/making-comparisons#ignoring-case) 
- [Hvordan håndtere nullverdier i Kotlin](https://www.baeldung.com/kotlin-null-safety)