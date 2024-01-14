---
title:    "Kotlin: Å lese kommandolinje-argumenter"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Hvorfor

Kommandolinje-argumenter er en viktig del av å lage effektive og brukervennlige programmer, spesielt når man arbeider med større prosjekter eller utvikler kommandolinje-apper. Ved å lære hvordan man leser kommandolinje-argumenter i Kotlin, kan du gjøre programmene dine mer fleksible og robuste.

# Hvordan gjøre det

Det første vi må gjøre er å importere "args"-variabelen som inneholder alle kommandolinje-argumentene som er gitt til programmet ditt. Deretter kan du bruke en for-løkke for å gå gjennom listen over argumenter og gjøre det du ønsker med dem. La oss se på et eksempel:

```Kotlin
fun main(args: Array<String>) {
    for (arg in args) {
        println("Argument: $arg")
    }
}
```

La oss anta at du kjører dette programmet fra kommandolinjen med følgende argumenter: "test1 test2 test3". Konsollen vil da vise følgende output:

```
Argument: test1
Argument: test2
Argument: test3
```

Som du kan se, vil for-løkken gå gjennom hvert argument og skrive det ut. Dette er bare et enkelt eksempel, du kan gjøre mye mer med kommandolinje-argumenter avhengig av hva slags app du lager.

# Dykk dypere

Nå som du har fått en forståelse av hvordan man leser kommandolinje-argumenter i Kotlin, kan det være nyttig å vite mer om hvordan man kan håndtere forskjellige typer argumenter som tall eller tekst. Du kan også utforske hvordan man bruker Kotlin-built-in-biblioteker for å analysere og validere argumenter.

Merk at hvis du kjører et Kotlin-program fra en integrert utviklingsmiljø (IDE), må du legge inn argumentene manuelt i konfigurasjonen av prosjektet ditt for at de skal bli tatt med.

# Se også

- [Offisiell Kotlin dokumentasjon om kommandolinje-argumenter](https://kotlinlang.org/docs/command-line.html)
- [Kotlin Cheat Sheet](https://kotlinlang.org/docs/tutorials/command-line.html)
- [Kotlin for Java-utviklere](https://kotlinlang.org/docs/tutorials/kotlin-for-java-developers.html)