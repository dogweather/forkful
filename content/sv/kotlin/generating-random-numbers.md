---
title:                "Generera slumpmässiga tal"
date:                  2024-01-20T17:49:42.633575-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generera slumpmässiga tal"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att generera slumpmässiga tal är precis vad det låter som - en process för att skapa tal som inte kan förutsägas på förhand. Programmerare använder detta för att addera osäkerhet i spel, simuleringar och säkerhetssystem för att göra resultaten mer oförutsägbara och därmed mer realistiska eller säkrare.

## Hur gör man:
Kotlin gör det lättpeasy att jobba med slumptal. Här är några grundexempel:

```Kotlin
import kotlin.random.Random

fun main() {
    // Skapa ett slumpmässigt heltal mellan 0 och 99
    val randomInt = Random.nextInt(100)
    println(randomInt) // T.ex. 42

    // Skapa ett slumpmässigt flyttal mellan 0.0 och 1.0
    val randomDouble = Random.nextDouble()
    println(randomDouble) // T.ex. 0.12345678901234568

    // Skapa en slumpmässig boolesk värde
    val randomBoolean = Random.nextBoolean()
    println(randomBoolean) // T.ex. true
}
```

## Djupdykning:
En gång i tiden användes fysiska metoder för att skapa slumptal, såsom tärningar och lotterihjul. I datorsammanhang är "slumpmässiga" tal oftast pseudoslumptal, genererade med en bestämd algoritm. De är inte helt oförutsägbara, men tillräckligt för många ändamål.

Det finns också s.k. verkliga slumptalgeneratorer (True Random Number Generators, TRNGs) som använder fysiska fenomen, såsom atomär oregelbundenhet, för att skapa verkligt oförutsägbara tal. Men i vanliga programmeringssammanhang räcker pseudoslumptal.

Kotlin använder klassen `Random` för pseudoslumptalsgenerering. `Random` kan producera olika typer av tal och till och med sekvenser. Dock, kom ihåg att om du behöver hög säkerhetsnivå, som i kryptografi, ska du använda säkrare källor för slumpmässighet såsom Java's `SecureRandom`.

## Se Även:
- Kotlin's officiella dokumentation för `Random` klassen: [Kotlin Random](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/)
- För mer om pseudoslumptal vs. verkliga slumptalgeneratorer, läs [Pseudorandom number generator - Wikipedia](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)
- Om du är intresserad av kryptografiskt säkra slumptal, kolla in [SecureRandom - Oracle's Java Documentation](https://docs.oracle.com/javase/8/docs/api/java/security/SecureRandom.html)
