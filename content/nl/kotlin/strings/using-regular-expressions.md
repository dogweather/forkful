---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:46.145566-07:00
description: "Reguliere expressies (regex) zijn hulpmiddelen voor het matchen van\
  \ patronen in tekst. Programmeurs gebruiken ze om data effici\xEBnt te zoeken, valideren\
  \ of\u2026"
lastmod: '2024-02-25T18:49:48.097094-07:00'
model: gpt-4-0125-preview
summary: "Reguliere expressies (regex) zijn hulpmiddelen voor het matchen van patronen\
  \ in tekst. Programmeurs gebruiken ze om data effici\xEBnt te zoeken, valideren\
  \ of\u2026"
title: Reguliere expressies gebruiken
---

{{< edit_this_page >}}

## Wat & Waarom?
Reguliere expressies (regex) zijn hulpmiddelen voor het matchen van patronen in tekst. Programmeurs gebruiken ze om data efficiënt te zoeken, valideren of manipuleren.

## Hoe te:
Kotlin maakt regex eenvoudig. Laten we enkele praktische codevoorbeelden bekijken:

```Kotlin
fun regexFind() {
    val pattern = "Kotlin".toRegex()
    val text = "Leren met Kotlin is leuk!"
    val matchResult = pattern.find(text)
    println(matchResult?.value) // Uitvoer: Kotlin
}

fun regexReplace() {
    val regex = "\\d+".toRegex()
    het adres = "123 Main Street"
    val sanitizedAddress = regex.replace(adres, "###")
    println(sanitizedAddress) // Uitvoer: ### Main Street
}

fun regexValidate() {
    val passwordPattern = "^(?=.*[A-Za-z])(?=.*\\d)[A-Za-z\\d]{8,}$".toRegex()
    val wachtwoord = "Password123"
    val isPasswordValid = passwordPattern.matches(wachtwoord)
    println(isPasswordValid) // Uitvoer: true
}

regexFind()
regexReplace()
regexValidate()
```

## Diepere Duik
Regex is sinds de jaren 1950 een basis in het programmeren, uitgevonden door wiskundige Stephen Kleene. Alternatieven voor regex omvatten string-methoden zoals `contains`, `startsWith`, of `split`, maar die zijn minder krachtig. Kotlin regex is gebouwd op Java's `Pattern` en `Matcher` klassen, waardoor het robuuste prestaties en nut biedt.

## Zie Ook
- Kotlin Docs over Regex: [kotlinlang.org](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)
- Regex Testgereedschap: [regex101.com](https://regex101.com/)
- Regex Handleiding: [regular-expressions.info](https://www.regular-expressions.info/tutorial.html)
