---
title:    "Kotlin: Utskrift av felsökningsutdata"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför

Att skriva kod kan ibland vara en frustrerande uppgift. Felmeddelanden dyker upp och felsökning kan ta lång tid. Ett sätt att effektivisera denna process är genom att använda utskrift av felsökningsmeddelanden, även kallat debug output. Detta gör det enklare att förstå vad som händer i koden och var felet kan ligga, vilket sparar både tid och huvudvärk.

## Så här gör du

För att skriva ut debug output i Kotlin kan du använda funktionen `println()` eller `print()`. Dessa kan du sedan placera var som helst i din kod för att få utskrifter på olika ställen.

```
Kotlin
fun main() {
    val num1 = 5
    val num2 = 10
    println(num1 + num2)
}
```
Output:
```
15
```

Det är också möjligt att skriva ut värden av variabler genom att inkludera dem i utskriften.
```
Kotlin
fun main() {
    val num1 = 5
    val num2 = 10
    println("Summan av $num1 och $num2 är ${num1 + num2}.")
}
```
Output:
```
Summan av 5 och $num2 är 15.
```

Du kan även använda debug output för att se värdet av variabler i olika stadier av ditt program. Till exempel kan du lägga till utskrifter innan och efter en loop för att se vilka värden variabeln har i varje steg.

```
Kotlin
fun main() {
    for (i in 1..10) {
        println("Variabeln i har värdet $i.")
    }
}
```
Output:
```
Variabeln i har värdet 1.
Variabeln i har värdet 2.
Variabeln i har värdet 3.
Variabeln i har värdet 4.
Variabeln i har värdet 5.
Variabeln i har värdet 6.
Variabeln i har värdet 7.
Variabeln i har värdet 8.
Variabeln i har värdet 9.
Variabeln i har värdet 10.
```

## Djupdykning

Debug output är en viktig del av felsökning och kan hjälpa dig att förstå vad som händer i ditt program. Genom att använda olika utskrifter kan du se hur värden förändras och hur din kod beter sig. Detta kan vara avgörande för att hitta och åtgärda fel i din kod.

Det är dock viktigt att tänka på att debug output endast bör användas under utvecklingsprocessen och inte i en färdig produktionsversion. Detta eftersom det kan påverka prestandan och säkerheten för din applikation.

## Se även

- [Kotlin officiell dokumentation om debugging](https://kotlinlang.org/docs/tutorials/debugging.html)
- [Ökat produktivitet genom felsökning i Kotlin](https://blog.jetbrains.com/kotlin/2017/04/debugging-in-kotlin/)