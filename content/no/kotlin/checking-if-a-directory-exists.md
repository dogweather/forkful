---
title:    "Kotlin: Sjekke om en mappe eksisterer"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hvorfor

Undersøke om en mappe finnes kan være nyttig når du jobber med Kotlin, spesielt når du skal håndtere filer og mapper i programmet ditt. Det kan hjelpe deg å forhindre feil eller unødvendig arbeid i ditt program.

## Hvordan

For å sjekke om en mappe finnes i Kotlin, kan du bruke følgende kode:

```Kotlin
val mapp = File("/sti/til/mappe")
if (mapp.exists()) {
    println("Mappen finnes!")
} else {
    println("Mappen finnes ikke.")
}
```

Dette vil sjekke om mappen eksisterer og skrive ut enten "Mappen finnes!" eller "Mappen finnes ikke." avhengig av resultatet. Du kan også bare bruke `exists()` funksjonen for å få en boolean som svar, og deretter gjøre andre handlinger basert på dette resultatet.

## Dypdykk

Å sjekke om en mappe finnes i Kotlin involverer å bruke `exists()` funksjonen fra `File` klassen. Denne funksjonen returnerer en boolean som viser om mappen faktisk eksisterer eller ikke. Det er også mulig å bruke `isDirectory()` funksjonen for å sjekke om det er en mappe eller en fil. Ved å kombinere disse to funksjonene, kan du få detaljert informasjon om en mappe og bruke dette i ditt program.

## Se også

- [Offisiell Kotlin dokumentasjon for File-klassen](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/index.html)
- [En enkel guide til fil og mappe håndtering i Kotlin](https://kotlinlang.org/docs/tutorials/kotlin-for-py/files.html)