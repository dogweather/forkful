---
aliases:
- /it/kotlin/converting-a-string-to-lower-case/
date: 2024-01-20 17:38:40.699836-07:00
description: "Convertire una stringa in minuscolo significa trasformare tutti i caratteri\
  \ alfabetici in lettere minuscole. I programmatori lo fanno per uniformare i\u2026"
lastmod: 2024-02-18 23:08:55.833972
model: gpt-4-1106-preview
summary: "Convertire una stringa in minuscolo significa trasformare tutti i caratteri\
  \ alfabetici in lettere minuscole. I programmatori lo fanno per uniformare i\u2026"
title: Conversione di una stringa in minuscolo
---

{{< edit_this_page >}}

## What & Why?
Convertire una stringa in minuscolo significa trasformare tutti i caratteri alfabetici in lettere minuscole. I programmatori lo fanno per uniformare i dati, specialmente per i confronti tra stringhe o per l'input utente.

## How to:
```kotlin
fun main() {
    val exampleString = "Ciao Mondo!"
    val lowerCaseString = exampleString.lowercase()
    println(lowerCaseString) // Output: ciao mondo!
}
```

## Deep Dive
La funzione `lowercase()` in Kotlin è stata introdotta come sostituta di `toLowerCase()` per migliorare la leggibilità e l'aderenza alle convenzioni di denominazione di Kotlin. La funzione tiene conto delle regole di localizzazione quando trasforma lettere maiuscole in minuscole.

Per esempio, la 'I' in inglese viene convertita in 'i' minuscolo, ma in turco diventa 'ı' (senza punto). Ecco un esempio che utilizza la localizzazione turca:

```kotlin
fun main() {
    val exampleString = "GELİŞTİRİCİ"
    val lowerCaseString = exampleString.lowercase(Locale.forLanguageTag("tr"))
    println(lowerCaseString) // Output: geliştirici
}
```

La decisione di usare la localizzazione predefinita o una specifica dipende dal contesto in cui si lavora. Se i dati devono essere coerenti indipendentemente dalla lingua dell'utente, potrebbe essere meglio specificare una localizzazione.

Ci sono alternative, come:

- `toUpperCase()`: per convertire in maiuscolo.
- `capitalize()`: deprecated, inizialmente utilizzato per maiuscolizzare la prima lettera.

L'implementazione della funzione `lowercase()` può variare in base alla piattaforma JVM o al sistema operativo, ma il risultato rimane conforme alle regole Unicode.

## See Also
- Kotlin Standard Library documentation: [String.lowercase](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/lowercase.html)
- Unicode Case Folding: [Case Folding Properties](http://www.unicode.org/Public/UCD/latest/ucd/CaseFolding.txt)
