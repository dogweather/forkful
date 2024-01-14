---
title:    "Kotlin: Utmatning av felsökningsutskrift"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Varför

Debuggning är en viktig del av programmering och att kunna skriva ut information för att spåra buggar är ett värdefullt verktyg för utvecklare. Utskrifter av debug-beräkningar kan ge oss en bättre förståelse för vad som händer i koden under exekveringen och möjliggöra felsökning av problem.

## Hur man gör det

För att skriva ut debug-utdata i Kotlin kan vi använda en enkel utskriftsåtgärd med hjälp av `println()` funktionen. Denna funktion skriver ut en sträng som kan innehålla variabler, värden eller annan information som vi vill övervaka.

```Kotlin
var num = 10
println("Värdet av variabeln num är: $num")
```

I detta exempel använder vi variabeln `num` i vår utskrift för att se värdet som den håller. Resultatet av denna kod skulle vara "Värdet av variabeln num är: 10".

Vi kan också använda `print()` funktionen för att skriva ut en sträng utan att lägga till en ny rad. Detta kan vara användbart om vi vill skriva ut flera värden på samma rad.

```Kotlin
var a = 5
var b = 10
print("$a + $b = ")
print(a + b)
```

I detta exempel kommer utskriften att vara "5 + 10 = 15".

Om vi vill ha mer kontroll över vår debug-utdata kan vi använda `Log` -klassen från Android SDK. Detta ger oss flera möjligheter att strukturera och filtrera våra utskrifter, vilket kan vara användbart vid felsökning.

```Kotlin
val TAG = "Debug"
var num = 5
Log.i(TAG, "Värdet av variabeln num är: $num")
```

I detta exempel använder vi `Log.i()` funktionen för att skriva ut information med taggen "Debug". Konsolresultatet skulle vara "I/Debug: Värdet av variabeln num är: 5".


## Djupdykning

När vi skriver ut debug-utdata är det viktigt att vara medveten om prestandaskapande kostnader. Att skriva ut för mycket information kan leda till långsammare exekvering av vårt program. Därför bör vi bara skriva ut det mest relevanta för att undvika onödiga avbrott.

En annan viktig sak att komma ihåg är att ta bort eller kommentera ut våra debug-utskrifter innan vi släpper vårt program. Lämna inte kvar några utskrifter i produktionskoden, eftersom det kan utgöra en säkerhetsrisk eller orsaka onödig overhead.

## Se även

- [Kotlin for Android Developers] (https://antonioleiva.com/kotlin-for-android-developers/) av Antonio Leiva
- [Debugging in Kotlin] (https://kotlinlang.org/docs/reference/using-gradle.html) på Kotlin's officiella dokumentationssida