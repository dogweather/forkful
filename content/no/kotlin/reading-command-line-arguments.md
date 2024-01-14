---
title:                "Kotlin: Leser kommandolinje-argumenter"
simple_title:         "Leser kommandolinje-argumenter"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hvorfor

Å lese kommandolinjeargumenter kan være svært nyttig for å lage programvare som er mer brukervennlig og fleksibel. Ved å kunne ta imot informasjon direkte fra brukeren, kan du tilpasse programmets funksjon basert på deres behov. I denne bloggen vil vi se på hvordan du enkelt kan lese kommandolinjeargumenter i Kotlin.

## Hvordan

Først må vi importere nødvendige pakker, som `args` fra `Array`:

```Kotlin
import java.util.Arrays

fun main(args: Array<String>) {
    // kode her
}

```

Deretter kan vi bruke `args` for å få tilgang til kommandolinjeargumentene. La oss si at vi vil lese en streng og et tall fra brukeren:

```Kotlin
val inputString = args[0] // første kommandolinjeargument
val inputNumber = args[1].toInt() // andre kommandolinjeargument, konvertert til et tall ved hjelp av .toInt() funksjonen
println("Du skrev inn $inputString og $inputNumber") // utskrift: Du skrev inn [inputString] og [inputNumber]
```

Du kan også benytte deg av `args.size` for å sjekke hvor mange argumenter som er gitt av brukeren.

## Dypdykk

Det er viktig å huske at kommandolinjeargumenter alltid er strenger, selv om du ønsker å lese inn tall. Derfor må du konvertere dem til riktig datatype ved å bruke funksjoner som `.toInt()` eller `.toDouble()`.

I tillegg går det an å lese inn flere argumenter ved hjelp av en `for`-løkke og `args.forEach`-funksjonen:

```Kotlin
args.forEach { arg ->
    println(arg) // utskrift: hvert kommandolinjeargument én etter én
}
```

Du kan også lese argumenter ved hjelp av flagg, for eksempel `--name` og `--age`:

```Kotlin
args.forEachIndexed { index, arg ->
    if(arg == "--name") {
        val name = args[index+1] // navnet vil være plassert i argumentet etter flagget
        println("Navn: $name")
    } else if(arg == "--age") {
        val age = args[index+1].toInt() // alderen vil være plassert i argumentet etter flagget, må konverteres til et tall
        println("Alder: $age")
    }
}
```

## Se også

- [Official Kotlin documentation on command line arguments](https://kotlinlang.org/docs/reference/command-line.html)
- [Tutorialspoint article on handling command line arguments in Kotlin](https://www.tutorialspoint.com/kotlin/kotlin_command_line_arguments.htm)
- [StackOverflow thread on reading command line arguments in Kotlin](https://stackoverflow.com/questions/4604237/how-to-process-console-arguments-in-kotlin)