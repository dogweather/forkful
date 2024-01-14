---
title:                "Kotlin: Slå ihop strängar"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför

I Kotlin-programmering är det väldigt vanligt att behöva kombinera flera strängar för att skapa en längre sträng. Detta kan vara till exempel för att skapa en textsträng som ska skrivas ut i en logg eller för att visa en dynamiskt genererad sträng i en användargränssnitt. I den här bloggposten kommer vi att titta närmare på hur man kan kombinera strängar i Kotlin.

## Hur man gör det

För att kombinera flera strängar i Kotlin använder man operatorn `+` eller funktionen `plus()`. Operatorn `+` används för att lägga till två strängar och returnerar en ny sträng som innehåller båda de ursprungliga strängarna. Här är ett exempel på hur man använder operatorn `+` för att kombinera två strängar:

```Kotlin
val firstName = "Johan"
val lastName = "Andersson"
val fullName = firstName + " " + lastName
```

I det här exemplet kombineras strängarna "Johan" och "Andersson" för att skapa den längre strängen "Johan Andersson". Det är viktigt att notera att det finns ett extra mellanslag mellan förnamn och efternamn i kombineringssträngen. Detta beror på att inget mellanslag ingår i variablerna firstName och lastName och måste därför läggas till manuellt.

Man kan också använda funktionen `plus()` för att kombinera flera strängar. Funktionen `plus()` tar en parameter av typen `String` och returnerar en ny sträng som kombinerar den befintliga strängen med den angivna parametern. Här är samma exempel som ovan, fast med användning av `plus()`:

```Kotlin
val firstName = "Johan"
val lastName = "Andersson"
val fullName = firstName.plus(" ").plus(lastName)
```

## Djupdykning

När man kombinerar flera strängar med operatorn `+` skapas en ny sträng varje gång. Detta kan bli ineffektivt vid större mängder data och orsaka onödig minnesallokering. För att undvika detta kan man istället använda sig av funktionen `StringBuilder`. `StringBuilder` är en effektivare metod att kombinera strängar eftersom den inte skapar en ny sträng varje gång man lägger till data, utan istället bygger på en befintlig sträng. Här är ett exempel på hur man kan använda `StringBuilder` för att kombinera flera strängar:

```Kotlin
val firstName = "Johan"
val lastName = "Andersson"
val fullName = StringBuilder(firstName)
        .append(" ")
        .append(lastName)
        .toString()
```

Det är viktigt att avsluta med att anropa `toString()`-funktionen för att få ut en slutgiltig sträng från `StringBuilder`.

## Se även

- [Kotlin String Interpolation](https://kotlinlang.org/docs/strings.html#string-interpolation)
- [Google Kotlin style guide](https://developer.android.com/kotlin/style-guide)