---
title:                "Interpolering av en streng"
html_title:           "Bash: Interpolering av en streng"
simple_title:         "Interpolering av en streng"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Strenginterpolering lar deg sette variabler rett inn i en tekststreng. Det erstatter kodesegmenter beskrevet mellom spesifikke tegn med variabelverdier. Det er en hendig teknikk for å gjøre kode mer lesbar, redusere antall feil og forbedre effektiviteten.

## Hvordan:

Her er en enkelt kodeblokk i Kotlin som viser hvordan man bruker strenginterpolering.

```Kotlin
val navn = "Per"
println("Hei, $navn!")
```
Resultatet av koden over vil bli:

```
Hei, Per!
```
Du kan også bruke uttrykk i strenginterpoleringen i Kotlin ved å innkapsle dem i krøllparenteser `{}` :

```Kotlin
val alder = 30
println("Om fem år, vil du være ${alder + 5} år.")
```
Resultatet av den ovennevnte koden vil være:

```
Om fem år, vil du være 35 år.
```

## Dypdykk:

Strenginterpolering har en lang historie og blir brukt i mange programmeringsspråk, inkludert Perl, Python, Ruby, og selvfølgelig Kotlin. Det er et kraftig verktøy som lar utviklere skrive mer forståelig kode.

Det er et par alternativer til strenginterpolering. Du kan for eksempel bruke konkatenation, men det kan gjøre koden mindre lesbar. Strengformatering er et annet alternativ, men det kan være mer krevende.

Interpoleringen i Kotlin fungerer ved at kompilatoren bak kulissene oversetter de interpolerte strengene til en sekvens av strengoperasjoner, noe som eliminerer behovet for manuell konvertering.

## Se også:

[Tutorial: Kotlin String Interpolation](https://kotlinlang.org/docs/basic-syntax.html#using-string-templates) - Offisiell Kotlin-dokumentasjon om strenginterpolering.
[String Interpolation in Kotlin](https://www.geeksforgeeks.org/string-interpolation-in-kotlin/) - En mer inngående guide til strenginterpolering i Kotlin fra GeeksforGeeks.