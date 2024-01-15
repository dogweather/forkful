---
title:                "Generering av slumpmässiga tal"
html_title:           "Kotlin: Generering av slumpmässiga tal"
simple_title:         "Generering av slumpmässiga tal"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför 

Randoma nummer behövs för att skapa variation och slumpmässighet i program och spel. Dessutom kan det vara ett användbart verktyg för att testa och simulera olika scenarier i kod.

## Hur man gör 

För att generera slumpmässiga nummer i Kotlin, används funktionen "random()". Detta returnerar ett nummer mellan 0 och 1.

```Kotlin
val randomNumber = random()
```

För att få ett nummer inom ett visst intervall, används "nextInt()" funktionen tillsammans med det önskade intervallet som parameter. I följande exempel kommer ett slumpmässigt nummer att genereras mellan 1 och 10.

```Kotlin
val randomNumber = nextInt(10) + 1
```

Om du vill generera slumpmässiga flyttal, används "nextDouble()" funktionen.

```Kotlin
val randomDecimal = nextDouble()
```

Slutligen, för att få ett slumpmässigt heltal inom ett visst intervall, kan "nextInt()" användas tillsammans med både start- och slutvärde som parametrar.

```Kotlin
val randomNumber = nextInt(5, 10)
```

## Deep Dive 

För att generera slumpmässiga nummer i Kotlin, används Mersenne Twister-algoritmen som är en av de mest använda algoritmerna för slumpmässighet. Detta beror på dess höga hastighet och periodicitet. Enligt dokumentationen för Kotlin, genererar Mersenne Twister-algoritmen ett 32-bitars binärt tal som sedan konverteras till ett flyttal mellan 0 och 1.

## Se även 

- Kotlin's Random class documentation: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/index.html
- Mersenne Twister algorithm: https://en.wikipedia.org/wiki/Mersenne_Twister