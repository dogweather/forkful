---
title:                "Kotlin: Å lese kommandolinjeargumenter"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hvorfor

Å lese kommandolinje-argumenter er viktig for å kunne ta inn og behandle brukerinput når du utvikler dine egne programmer. Det gjør det mulig for deg å gjøre programmene dine mer interaktive og gi brukere mulighet til å tilpasse deres opplevelse.

## Hvordan

Å lese kommandolinje-argumenter i Kotlin er enkelt og kan gjøres med bare noen få linjer med kode. Først må du importere pakken for å kunne lese argumenter:
```
import kotlin.system.exitProcess
```
Deretter kan du bruke en ```main``` funksjon for å lese argumentene som ble gitt ved å kjøre programmet:
```
fun main(args: Array<String>) {
    // kode for å lese kommandolinje-argumenter
}
```
For å faktisk lese argumentene, kan du bruke ```args``` variabelen og iterere gjennom den ved hjelp av en ```for``` løkke:
```
for (arg in args) {
    // behandle hver argument individuelt
}
```
Du kan også få tilgang til argumentene ved å bruke indeksering på ```args``` variabelen:
```
val førsteArg = args[0] // gir første argument
```
Når du har fått tak i argumentene, kan du bruke dem til å styre programmet ditt og tilpasse funksjonaliteten basert på brukerens input.

## Deep Dive

Etter å ha brukt kommandolinje-argumenter i Kotlin en stund, vil du kanskje begynne å lure på hva som egentlig skjer bak kulissene. Når du kjører et Kotlin program fra kommandolinjen, blir argumentene som ble gitt skilt fra hverandre med mellomrom og lagret i en array. Programmet kaller deretter ```main``` funksjonen og sender denne argument-arrayen som et argument. Derfra kan du få tilgang til hvert argument individuelt som nevnt tidligere. Det er viktig å huske at argumentene blir tolket som strenger, og du må gjøre konverteringer hvis du ønsker å bruke dem som tall eller andre data-typer.

## Se også

Her er noen nyttige ressurser for å lære mer om å lese kommandolinje-argumenter i Kotlin:

- Offisiell Kotlin dokumentasjon for å lese argumenter: https://kotlinlang.org/docs/command-line.html
- En dypere forklaring på hva som skjer bak kulissene når du leser kommandolinje-argumenter: https://stackoverflow.com/a/26252427
- Eksempler på å implementere argument-lesing i et reelt Kotlin-prosjekt: https://medium.com/the-coding-matrix/passing-and-handling-command-line-arguments-in-kotlin-e96e533b5945