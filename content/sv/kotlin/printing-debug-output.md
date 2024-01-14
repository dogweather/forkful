---
title:                "Kotlin: Utskrift av felsökningsutdata"
programming_language: "Kotlin"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför

I många programmeringsprojekt är det ofta nödvändigt att felsöka och hitta buggar i koden. Då är det viktigt att kunna använda en effektiv metod för att felsöka och förstå vad som händer i koden. Det är här utskrift av debug-utdata kommer in i bilden. Genom att skriva ut värden och variabler vid olika stadier av programkörningen kan vi enklare spåra och identifiera eventuella fel.

## Hur man gör

För att skriva ut debug-utdata i Kotlin kan vi använda oss av funktionen `println()`. Detta gör att vi kan skriva ut text och värden direkt till konsolen. Här är ett exempel på hur vi kan använda oss av detta:

```Kotlin
// Skriver ut ett meddelande
println("Hej världen!")

// Skriver ut värdet av en variabel
val namn = "Sara"
println(namn)

// Skriver ut resultatet av en beräkning
val summa = 5 + 3
println("Summan är $summa")
```

Det är också möjligt att skriva ut debug-utdata till en fil istället för konsolen. Detta kan vara användbart om du behöver spara utdatan för senare analys. Detta kan göras genom att använda en `PrintWriter` och skriva ut till en `File`:

```Kotlin
val utmatningFilstig = File("debug-utmatning.txt")
val printWriter = PrintWriter(utmatningFilstig) 

// Skriver ut till filen
printWriter.println("Felet har inträffat i linje 10")
printWriter.close()
```

## Fördjupning

Att använda utskrift av debug-utdata är ett effektivt sätt att felsöka och förstå din kod. Genom att välja ut specifika värden och variabler att skriva ut vid olika punkter i koden kan du enklare spåra och fixa eventuella fel. Det är också bra att använda denna metod för att förbättra din förståelse för hur din kod fungerar.

En annan fördel med att använda utskrift av debug-utdata är att det kan vara till hjälp i teamarbete. Om flera personer arbetar på samma projekt kan utskrift av debug-utdata bidra till att snabba upp felsökningen och undvika onödig stress.

## Se även

Här är några andra resurser som kan vara till hjälp när du lär dig mer om utskrift av debug-utdata i Kotlin:

- [Debugging and logging in Kotlin](https://www.raywenderlich.com/6408-debugging-and-logging-in-kotlin)
- [Debugging Tool for Kotlin](https://blog.jetbrains.com/kotlin/2013/09/debugging-tools-for-kotlin/)
- [Kotlin cheat sheet](https://kotlinlang.org/docs/reference/idioms.html#logging)