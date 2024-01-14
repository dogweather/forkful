---
title:    "Kotlin: Generering av slumpmässiga tal"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför
Att generera slumpmässiga nummer är en vanlig uppgift inom programmering och kan vara användbart för allt från spel till simuleringar och kryptografi.

## Hur man gör det
För att generera slumpmässiga nummer i Kotlin, kan vi använda standardbibliotekets `Random` klass. Den här klassen har olika metoder för att generera olika typer av slumpmässiga nummer.

Här är ett exempel på hur man kan skapa en `Random` instans och sedan använda `nextInt()` metoden för att generera ett slumpmässigt heltal:

```Kotlin
val random = Random()
val randomNumber = random.nextInt()
println(randomNumber) // t.ex. 123456789
```

För att begränsa det slumpmässiga nummrets omfång, kan vi använda `nextInt(bound)` metoden för att generera ett nummer mellan 0 (inklusive) och `bound` (exklusive):

```Kotlin
val randomNumber = random.nextInt(100) // slumpmässigt nummer mellan 0 (inklusive) och 100 (exklusive)
```

Vi kan även använda `nextDouble()` metoden för att generera ett slumpmässigt flyttal mellan 0.0 (inklusive) och 1.0 (exklusive). Genom att sedan multiplicera detta med ett önskat intervall kan vi få ett slumpmässigt nummer inom detta intervall:

```Kotlin
val randomDouble = random.nextDouble() // slumpmässigt flyttal mellan 0.0 (inklusive) och 1.0 (exklusive)
val range = 10.0..20.0
val result = randomDouble * (range.endInclusive - range.start) + range.start // slumpmässigt nummer mellan 10.0 (inklusive) och 20.0 (inklusive)
println(result) // t.ex. 16.731548124
```

## Djupdykning
För att förstå hur `Random` klassen genererar slumpmässiga nummer kan vi titta närmare på dess implementering.

I grunden använder `Random` klassen en algoritm som heter *Linear Congruential Generator* (LCG). Denna algoritm använder en formel för att generera en följd av nummer. För att få en mer jämn och slumpmässig fördelning, kombinerar `Random` klassen detta med andra logiska operationer.

Det finns dock en viktig sak att notera - `Random` klassen är inte garanterat att generera helt slumpmässiga nummer eftersom den använder sig av en förutbestämd, seedad sekvens. Detta innebär att om vi skapar två olika `Random` instanser med samma seed kommer de att generera samma sekvens av nummer. För att få mer slumpmässiga resultat, kan vi istället använda den statiska `random()` metoden som ger oss en slumpmässig seed varje gång den anropas:

```Kotlin
val randomNumber = random() // slumpmässigt nummer baserat på en ny seed
```

## Se även
- [Kotlin `Random` dokumentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/)
- [Linear Congruential Generator](https://en.wikipedia.org/wiki/Linear_congruential_generator)