---
title:                "Utskrift av feilfinnende utdata"
html_title:           "Kotlin: Utskrift av feilfinnende utdata"
simple_title:         "Utskrift av feilfinnende utdata"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor skulle noen ønske å skrive ut feilsøkingsutdata (debug output)? Det er en god måte å finne ut hva som skjer i programmet ditt og for å feilsøke eventuelle problemer du støter på. 

## Hvordan gjøre det

For å skrive ut feilsøkingsutdata ganske enkelt, kan du bruke funksjonen `print()` eller `println()` i Kotlin. Denne funksjonen tar inn en parameter som du vil skrive ut.

```Kotlin
val navn = "Maria"
println(navn)
```

Dette vil skrive ut `Maria` i terminalen eller konsollen din når du kjører programmet ditt.

Det er også mulig å formatere utdataen din ved hjelp av "string templates" i Kotlin. For å gjøre dette, bruk `$` foran variabelnavnet eller `${}` foran uttrykket du vil skrive ut.

```Kotlin
val tall1 = 10
val tall2 = 5
val sum = tall1 + tall2

println("Summen av $tall1 og $tall2 er $sum.")
```

Dette vil skrive ut `Summen av 10 og 5 er 15.` i terminalen din.

## Dypdykk

For å skrive ut mer komplekse utdata, kan du bruke `debug()` funksjonen i Kotlin. Denne funksjonen tar inn en varargs parameter, slik at du kan skrive ut flere verdier samtidig.

```Kotlin
val tall = listOf(1, 2, 3, 4)
debug("Tallene er: ", *tall)
```

Dette vil skrive ut `Tallene er: [1, 2, 3, 4]` i terminalen din.

Det er også mulig å bruke `debug()` funksjonen for å skrive ut dine egne feilmeldinger. Dette kan være nyttig når du ønsker å feilsøke og finne ut hvor i koden din et problem oppstår.

```Kotlin
val navn = "Maria"
if (navn != "Maria") {
    debug("Feil navn, forventet Maria men fikk $navn.")
}
```

Dette vil skrive ut `Feil navn, forventet Maria men fikk Anna.` hvis `navn` er noe annet enn "Maria".

## Se også

- [Kotlin Documentation](https://kotlinlang.org/docs/reference/)
- [Debugging in Kotlin](https://kotlinlang.org/docs/tutorials/debugging.html)
- [Using print(), println(), and debug() in Kotlin](https://www.baeldung.com/kotlin/print-println-debug)