---
title:    "Kotlin: Sammanslagning av strängar"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Varför

När du programmerar i Kotlin kan du ibland behöva kombinera flera strängar för att skapa en enda sträng. Detta kallas för "concatenation" och används för att skapa mer dynamiska och anpassningsbara strängar. Genom att lära dig hur man konkatenerar strängar kan du skriva kod som blir mer läsbar och effektiv.

## Hur man gör

För att konkatenera strängar i Kotlin använder man sig av operatören "+" eller funktionen "plus". Här är ett exempel:

```Kotlin
val firstName = "Anna"
val lastName = "Andersson"

val fullName = firstName + " " + lastName
val completeName = firstName.plus(" ").plus(lastName)

println(fullName) //Output: Anna Andersson
println(completeName) //Output: Anna Andersson
```

I kodexemplet ovan har vi två variabler som innehåller förnamn och efternamn. Genom att använda "+" eller "plus" funktionen kan vi enkelt konkatenera dem till en ny variabel och få ut en sammanslagen sträng.

Det är också möjligt att använda "plus" funktionen för att konkatenera flera strängar samtidigt:

```Kotlin
val message = "Hej".plus(" ").plus("alla").plus("!")

println(message) //Output: Hej alla!
```

## Djupdyk

När det gäller att konkatenera strängar finns det några saker som är viktiga att tänka på. För det första är det viktigt att komma ihåg att strängar i Kotlin är oföränderliga (immutable). Det betyder att varje gång vi konkatenerar en sträng så skapas en helt ny sträng.

För att undvika att skapa onödigt många objekt när vi konkatenerar strängar, kan vi använda funktionen "StringBuilder". Denna funktion tillåter oss att bygga upp en sträng gradvis genom att lägga till delar av strängar istället för att skapa nya objekt varje gång. Här är ett exempel:

```Kotlin
val firstName = "Emma"
val lastName = "Eriksson"

val fullNameBuilder = StringBuilder(firstName)
fullNameBuilder.append(" ")
fullNameBuilder.append(lastName)

println(fullNameBuilder.toString()) //Output: Emma Eriksson
```

## Se även

Se gärna dessa länkar för mer information om att konkatenera strängar i Kotlin:

- [Officiell dokumentation](https://kotlinlang.org/docs/tutorials/basic-syntax.html#string-templates)
- [Konkatenera strängar i Kotlin på Stack Overflow](https://stackoverflow.com/questions/39608864/kotlin-how-to-concatenate-strings)
- [Video tutorial om strängar i Kotlin](https://www.youtube.com/watch?v=wRa58N6cOO4)