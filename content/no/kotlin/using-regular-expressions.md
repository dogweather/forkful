---
title:    "Kotlin: Å bruke regulære uttrykk"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hvorfor

Å bruke regulære uttrykk (regular expressions) er en utrolig nyttig programmeringsferdighet å ha i verktøykassen din. Det lar deg enkelt søke etter og manipulere tekst, noe som kan være svært nyttig når du jobber med store mengder data eller ønsker å finne spesifikke mønstre i en tekst. Med regulære uttrykk kan du utføre komplekse og presise søk i et strengformat, noe som kan spare deg for mye tid og frustrasjon.

## Slik gjør du det

For å bruke regulære uttrykk i Kotlin, må du importere Regex-biblioteket. Deretter kan du bruke en rekke metoder og operasjoner for å søke etter og manipulere tekst. La oss se på noen enkle eksempler:

```Kotlin
val text = "Jeg elsker å kode i Kotlin"
val regex = Regex("[aeiou]") //dette regulære uttrykket vil søke etter alle vokaler
val result = regex.findAll(text) //denne metoden returnerer alle matcher i teksten
println(result.count()) //output: 8, siden teksten inneholder 8 vokaler
```

Du kan også bruke regulære uttrykk for å endre tekst. Her er et eksempel der vi erstatter alle forekomster av "kode" med "programmer":

```Kotlin
val text = "Jeg elsker å kode i Kotlin"
val regex = Regex("kode") //dette regulære uttrykket vil matche "kode"
val result = regex.replace(text, "programmer") //denne metoden vil erstatte alle forekomster av "kode" med "programmer"
println(result) //output: "Jeg elsker å programmere i Kotlin"
```

Det er utallige muligheter når det kommer til bruk av regulære uttrykk, og det er viktig å lese dokumentasjonen og eksperimentere for å bli komfortabel med det.

## Dykk ned i det

Å forstå regulære uttrykk kan være litt forvirrende i begynnelsen, men med litt øvelse vil du snart mestre det. Det er viktig å lære de forskjellige symbolene og operatorer som brukes i regulære uttrykk, som f.eks. "[]" for å lage en karakterklasse eller "." for å matche et enkelt tegn.

Det er også viktig å være oppmerksom på metoder som "find" og "findAll" som returnerer forskjellige verdier og må brukes på riktig måte for å få ønsket resultat.

En annen ting som kan være nyttig å vite er at du kan bruke regulære uttrykk i de fleste programmeringsspråk, ikke bare Kotlin. Dette gjør det til en nyttig ferdighet å ha uansett hvilket språk du bruker.

## Se også

Her er noen lenker til ressurser som kan hjelpe deg videre i læringsprosessen:

- Dokumentasjon for Regex-biblioteket i Kotlin: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/
- En interaktiv tutorial om regulære uttrykk: https://regexone.com/
- En liste over vanlige symboler og operatorer i regulære uttrykk: https://www.rexegg.com/regex-quickstart.html

Lykke til med å bruke regulære uttrykk i Kotlin!