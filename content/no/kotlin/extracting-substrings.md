---
title:    "Kotlin: Uthenting av delstrenger"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor

Noen ganger i programmering er det nødvendig å hente ut deler av en tekststreng for å kunne jobbe videre med den. Dette kalles for å ekstrahere substrings. Dette kan være nyttig når man for eksempel trenger å finne et bestemt ord eller en del av et ord i en større tekst. 

## Slik gjør du det

For å ekstrahere substrings i Kotlin kan man bruke funksjonen "substring" som tar inn to parametere: start og sluttindeks. Eksempelvis kan vi gjøre følgende:

```Kotlin
val tekst = "Dette er en tekststreng"
val substring = tekst.substring(5,8)
println(substring) // output: er
```
I dette eksempelet har vi en tekststreng og ønsker å ekstrahere ordet "er". Siden "e" er på indeks 5 i strengen og "r" er på indeks 8, gir vi disse verdien som parametere til "substring" funksjonen. Vi printer så ut den ekstraherte substringen og får som ønsket resultatet "er". 

Dersom man ønsker å ekstrahere en del av et ord, kan man bruke funksjonen "indexOf" for å finne indeksene til start og slutt av den delen man vil ekstrahere. En annen måte å gjøre dette på er å bruke regulære uttrykk. For eksempel kan man bruke funksjonen "regex" og metoden "find" for å finne et bestemt ord i en tekststreng. 

## Dykk dypere

Det finnes flere metoder for å ekstrahere substrings i Kotlin, som for eksempel "substringBefore" og "substringAfter". Disse kan være nyttige dersom man ønsker å ekstrahere en del av en tekst før eller etter et spesifikt tegn eller ord.

En annen viktig ting å huske på er at indeksering i Kotlin starter på 0, altså at det første tegnet i en tekststreng har indeks 0. Dette kan være forvirrende for de som er vant til å jobbe med programmeringsspråk som starter indeksering på 1.

Det kan også være lurt å vurdere effektiviteten til substring funksjonen i forhold til minnebruk og ytelse, spesielt når man jobber med store tekststrenger.

## Se også

- [Kotlin documentation](https://kotlinlang.org/docs/reference/functions.html#substring) 
- [Tutorial: An Introduction to Kotlin Substring](https://www.baeldung.com/kotlin/substring) 
- [Kotlin String Cheat Sheet](https://programmingwithmosh.com/wp-content/uploads/2020/03/KotlinCheatSheet.pdf)