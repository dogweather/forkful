---
title:    "Kotlin: Lesing av kommandolinje-argumenter"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

# Hvorfor
Hvorfor bør man lese kommandolinje-argumenter når man programmerer? En vanlig grunn er for å gi programmet ditt ulik oppførsel eller funksjonalitet basert på hvilke argumenter som blir gitt under kjøring. Dette kan være nyttig når man ønsker å tilpasse programmet til ulike behov eller situasjoner.

# Hvordan
Det er flere måter å lese kommandolinje-argumenter i Kotlin på, men en vanlig metode er å bruke `args`-variabelen som er tilgjengelig som en parameter i `main()`-funksjonen.

```Kotlin
fun main(args: Array<String>) {
    if (args.isNotEmpty()) { // sjekker om det finnes argumenter
        val arg1 = args[0] // tar ut første argument
        println("Første argument: $arg1")
    } else {
        println("Ingen kommandolinje-argumenter ble gitt.")
    }
}
```

Kjører man dette programmet med kommandolinje-argumenter som f.eks. `java Program arg1 arg2`, vil man få følgende output:

```
Første argument: arg1
```

Man kan også bruke `System.getProperty()` for å lese spesifikke systemegenskaper som er satt ved kjøring av programmet.

```Kotlin
fun main(args: Array<String>) {
    val property = System.getProperty("navn")
    println("Hei, $property!")
}
```

Når man kjører dette programmet med argumentet `-Dnavn=Anne`, vil man få følgende output:

```
Hei, Anne!
```

Det finnes også biblioteker som gjør det enklere å håndtere kommandolinje-argumenter, som f.eks. `args4j` og `commons-cli`.

# Dypdykk
Det kan være nyttig å vite at kommandolinje-argumenter blir lest inn som strenger, derfor må man selv konvertere til ønsket datatype. Man bør også være klar over at argumenter som inneholder mellomrom må settes i anførselstegn for å bli lest riktig.

Det er også mulig å legge til egne argumenter og flagg som gir mer avansert funksjonalitet. Dette kan være nyttig når man ønsker å tilby kraftigere muligheter for å konfigurere programmet.

# Se Også
- [Kotlin sin offisielle dokumentasjon for kommandolinje-argumenter](https://kotlinlang.org/docs/reference/properties.html#command-line-arguments)
- [Eksempler på hvordan man kan bruke kommandolinje-argumenter i Kotlin](https://www.tutorialspoint.com/kotlin/command_line_arguments_in_kotlin.htm)
- [Biblioteket args4j for å håndtere kommandolinje-argumenter i Kotlin](https://github.com/kohsuke/args4j)