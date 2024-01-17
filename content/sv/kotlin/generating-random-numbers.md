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

## Vad & Varför?
Generering av slumpmässiga nummer är en viktig del av programmering, eftersom det tillåter oss att skapa variation och slumpmässighet i våra program. Detta är särskilt användbart när det kommer till spel, simuleringar, och kryptering.

## Så här gör du:
```Kotlin
// Genererar ett slumpmässigt heltal mellan 1 och 10
val slumpNumber = (1..10).random() 
println(slumpNumber) // Kan genereras olika varje gång programmet körs

// Genererar ett slumpmässigt flyttal mellan 0.0 och 1.0
val slumpTal = Math.random() 
println(slumpTal) // Kan genereras olika varje gång programmet körs
```

## Djupdykning:
Generering av slumpmässiga nummer är en viktig del av datavetenskapens historia, och algoritmer för detta har utvecklats sedan 1940-talet. Det finns olika sätt att generera slumpmässiga nummer, inklusive pseudo-slumpmässiga algoritmer (som den som används av Kotlin i exemplet ovan) och sann-slumpmässiga algoritmer som använder sig av yttre faktorer, som exempelvis brus från mikrofonen på din dator. Om du behöver generera slumpmässiga nummer med extra hög säkerhet, kan du överväga att använda en krypto-mappare.

## Se även:
- [Kotlin Random Dokumentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-kotlin.-random/)
- [Sualin Random (Sann-slumpmässig algoritm)](https://www.cs.princeton.edu/~rs/AlgsDS07/11RadomnessS71.pdf)