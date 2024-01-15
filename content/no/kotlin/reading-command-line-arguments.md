---
title:                "Leser kommandolinje-argumenter"
html_title:           "Kotlin: Leser kommandolinje-argumenter"
simple_title:         "Leser kommandolinje-argumenter"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hvorfor
Å lese kommandolinje-argumenter er en nøkkelkompetanse som alle Kotlin-programmerere bør ha. Det lar deg utvide funksjonaliteten til programmene dine ved å ta imot informasjon fra brukeren på kjøretid.

## Hvordan
Det første trinnet for å lese kommandolinje-argumenter i Kotlin er å importere `kotlin.text.Regex` biblioteket. Deretter kan du bruke `args` variabelen i `main` funksjonen din for å få tilgang til kommandolinje-argumentene som en matrise (array). Her er et eksempel på hvordan du bruker `args` variabelen for å skrive ut alle argumentene som ble passert inn ved å kjøre programmet:

```Kotlin
import kotlin.text.Regex

fun main(args: Array<String>) {
    println("Kommandolinje-argumenter:")
    for (argument in args) {
        println(Regex.escape(argument))
    }
}
```

Hvis du for eksempel kjører programmet med argumentene "Hello" og "World", vil følgende bli skrevet ut:

```
Kommandolinje-argumenter:
Hello
World
```

Du kan også få tilgang til en bestemt mengde argumenter ved å bruke deres indeks i `args` matrisen. For eksempel, hvis du bare ville skrive ut det første og siste argumentet, kunne du gjort noe som dette:

```Kotlin
import kotlin.text.Regex

fun main(args: Array<String>) {
    println("Første argument: " + args[0])
    println("Siste argument: " + args[args.size - 1])
}
```

Som før, hvis du kjører programmet med argumentene "Hello" og "World", vil følgende bli skrevet ut:

```
Første argument: Hello
Siste argument: World
```

## Deep Dive
Du kan også bruke Kotlin-built-in `argParser` for å lese og analysere kommandolinje-argumenter på en mer strukturert måte. Med dette verktøyet kan du enkelt definere forskjellige argumenttyper og deres verdier, og få tilgang til dem i koden din ved hjelp av argumentnavnene. Her er et eksempel på hvordan dette kan gjøres:

```Kotlin
import kotlin.system.exitProcess

fun main(args: Array<String>) {
    argParser(args).parseInto(::Config).runCatching {
        if (hasHelpOption) {
            println("Dette programmet tar imot to kommandolinje-argumenter: <navn> og <alder>")
            exitProcess(0)
        }
        println("Navn: $name")
        println("Alder: $age")
    }.onFailure {
        System.err.println("Ugyldige argumenter. Bruk '--help' for å se brukermenyen.")
        exitProcess(1)
    }
}

data class Config(val name: String, val age: Int, val hasHelpOption: Boolean)

fun Config.Companion.create() = Config("", 0, false) //Default verdier

private fun argParser(args: Array<String>) = if (args.contains("--help")) {
    Config.create().copy(hasHelpOption = true)
} else {
    Config.create().apply {
        val iterator = args.iterator()
        while (iterator.hasNext()) {
            when (iterator.next()) {
                "--name" -> name = iterator.next() //Argumentnavn og -verdi
                "--age" -> age = iterator.next().toInt() //Argumentnavn og -verdi konvertert til Int
                else -> throw Exception() //Ugyldig argument
            }
        }
    }
}
```

Her definerer vi to typer argumenter, navn og alder, og bruker `argParser` til å hente og analysere disse argumentene fra kommandolinjen. Hvis brukeren inkluderer "--help" som et argument, vil vi bare skrive ut en hjelpemeny og avslutte programmet. Hvis ikke, vil vi få tilgang til og skrive ut navn og alder som ble angitt som argumenter. Her er noen eksempler på hvordan dette kan kjøres:

```
$ kotlin Main --help
Dette programmet tar imot to kommandolinje-argumenter: <navn> og <alder>

$ kotlin Main --name Alice --age 25
Navn: Alice
Alder