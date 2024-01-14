---
title:    "Kotlin: Generering av tilfeldige tall"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Hvorfor

Generering av tilfeldige tall er en viktig del av mange programmeringsoppgaver. Det kan være nyttig for å lage unike passord, simulere tilfeldige hendelser i et spill, eller generelt for å tilføre variasjon og uforutsigbarhet til et program. 

## Slik gjør du det

For å generere tilfeldige tall i Kotlin, kan du bruke funksjonen `random()`. Denne funksjonen returnerer et tilfeldig tall mellom 0 og 1. Det kan være lurt å bruke `Math.random()` for større tallområder. Se eksemplet nedenfor for å se hvordan dette kan implementeres:

```Kotlin
val tilfeldigTall = random()
println(tilfeldigTall)

val størreTall = (Math.random() * 100).toInt()
println(størreTall)
```

Dette vil gi forskjellige tilfeldige tall hver gang programmet kjøres. Du kan også legge til flere variabler og operasjoner for å få mer komplekse tilfeldige tall. 

## Dykk dypere

Å generere tilfeldige tall kan virke enkelt, men det er viktig å være klar over noen begrensninger og fallgruver. For eksempel er ikke tallene som genereres ved hjelp av `random()` helt tilfeldige, siden de bygger på en algoritme. Det betyr at hvis du kjører en loop som kaller `random()` flere ganger, vil du få en serie av tall som følger et visst mønster. 

En annen ting å huske på er å kontrollere hva slags tallområde du vil ha. Ved hjelp av `Math.random()` og operasjoner som `toInt()` og `toDouble()` kan du justere hvor stor eller liten spennvidden skal være for dine tilfeldige tall. 

## Se også

- [Offisiell Kotlin-dokumentasjon for `random()`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/random.html)
- [Generering av tilfeldige tall i Kotlin](https://www.callicoder.com/kotlin-random-util-functions/)
- [Les mer om tilfeldighet og algoritmer](https://betterexplained.com/articles/randomness/)