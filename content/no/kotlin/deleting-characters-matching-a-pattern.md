---
title:    "Kotlin: Slette tegn som matcher et mønster"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor

Noen ganger kan det være nyttig å fjerne spesifikke tegn fra en streng i Kotlin. Dette kan være for å formatere data eller for å oppfylle bestemte krav til input i en applikasjon.

## Slik gjør du det

For å fjerne tegn som matcher et bestemt mønster i en streng, kan du bruke metoden `replace` og en Regular Expression (Regex) i Kotlin. La oss si at vi vil fjerne alle bokstaver fra en streng:

```Kotlin
var tekst = "Dette er en tekst med ulike bokstaver"
tekst = tekst.replace(Regex("[a-zA-Z]"), "")
println(tekst)
```

Output:
```
        
             
        


## Fordypning

La oss gå litt dypere inn i hva som skjer i koden over. Metoden `replace` tar imot to parametere: et Regular Expression mønster og en ny verdi. I dette tilfellet bruker vi `[a-zA-Z]` som betyr alle bokstaver fra A-Z (både stor og liten). Denne koden vil dermed fjerne alle bokstaver og output blir en tom streng.

Men hva om vi vil beholde noen bokstaver og fjerne resten? Da kan vi legge til de bokstavene vi vil beholde i mønsteret med et `|` mellom dem. For eksempel, hvis vi vil beholde A, E og I, men fjerne resten av bokstavene, kan vi bruke dette mønsteret: `[a-ei-z]`. Da vil koden se slik ut:

```Kotlin
var tekst = "Dette er en tekst med ulike bokstaver"
tekst = tekst.replace(Regex("[a-ei-z]"), "")
println(tekst)
```

Output:
```
A E I           
             
```

Som du kan se, er bokstavene A, E og I beholdt, mens resten er fjernet.

## Se også

- [Metoden `replace` dokumentasjon i Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/replace.html)
- [Regex i Kotlin: A complete guide](https://kotlinlang.org/docs/reference/regular-expressions.html)
- [Kotlin Strings: A Complete Guide](https://kotlinlang.org/docs/reference/basic-types.html#strings)