---
title:    "Kotlin: Sjekke om en mappe eksisterer"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Hvorfor

Det er viktig å sjekke om en mappe eksisterer i et program for å sikre korrekt håndtering av filer og opprettholde effektivitet i koden din.

## Hvordan

Vi kan sjekke om en mappe eksisterer i Kotlin ved hjelp av File-objektet og dens `exists()` metode. Her er et eksempel på hvordan dette kan gjøres:

```Kotlin
val mappe = File("sti/til/mappe")
if(mappe.exists()){
    println("Mappen finnes")
} else {
    println("Mappen finnes ikke")
}
```

Dette vil skrive ut enten "Mappen finnes" eller "Mappen finnes ikke" avhengig av om mappen eksisterer eller ikke. Det er også viktig å merke seg at denne metoden returnerer `true` selv om den refererte filen er en fil og ikke en mappe.

## Dypdykk

Vi kan også sjekke om en mappe eksisterer ved å bruke `listFiles()` metoden i File-objektet. Dette vil returnere en liste over alle filer og mapper i den refererte mappen. Hvis mappen ikke eksisterer, vil denne metoden returnere `null`. Her er et eksempel på hvordan dette kan implementeres:

```Kotlin
val mappe = File("sti/til/mappe")
if(mappe.listFiles() != null){
    println("Mappen finnes")
} else {
    println("Mappen finnes ikke")
}
```

Dette gir deg også muligheten til å få tilgang til alle filene og mappene i den refererte mappen for videre behandling i koden din.

## Se også

- [Kotlin dokumentasjon for File-objektet](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/)
- [Java dokumentasjon for File-objektet](https://docs.oracle.com/javase/7/docs/api/java/io/File.html#exists())
- [En tutorial om filbehandling i Kotlin](https://javarevisited.blogspot.com/2017/07/how-to-create-read-write-or-delete-file-kotlin-java-example.html)