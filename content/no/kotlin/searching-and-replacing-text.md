---
title:    "Kotlin: Søke og erstatte tekst"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor

Å søke og erstatte tekst i kode er en viktig ferdighet for enhver programmerer. Det lar deg raskt gjøre endringer i store mengder kode, noe som sparer tid og reduserer risikoen for feil.

## Hvordan

Det første du må gjøre er å identifisere teksten du ønsker å erstatte. Dette kan være et enkelt ord eller en hel setning. Deretter kan du bruke Kotlin`s innebygde funksjon `replace()` for å erstatte teksten med det du ønsker.

```Kotlin
val originalTekst = "Hei, welcome to my blog!"
val nyTekst = originalTekst.replace("welcome", "velkommen")
println(nyTekst)

// output: "Hei, velkommen to my blog!"
```

I noen tilfeller vil du kanskje også ønske å gjøre et bredere søk ved hjelp av regulære uttrykk. Dette kan gjøres ved å bruke `replace()` med et regex-uttrykk som første parameter.

```Kotlin
val originalTekst = "I love coding in Kotlin and Java"
val nyTekst = originalTekst.replaceRegex("[Kotlin]+".toRegex(), "Swift")
println(nyTekst)

// output: "I love coding in Swift and Java"
```

## Dypdykk

Det er viktig å være klar over at `replace()`-funksjonen vil erstatte alle forekomster av teksten du søker etter. Hvis du ønsker å begrense erstatningen til en spesifikk del av teksten, kan du bruke den valgfrie parameteren `limit` som begrenser antall erstatninger.

```Kotlin
val originalTekst = "code code code code"
val nyTekst = originalTekst.replace("code", "Kotlin", limit = 2)
println(nyTekst)

// output: "Kotlin Kotlin code code"
```

Det finnes også forskjellige versjoner av `replace()`-funksjonen, for eksempel `replaceFirst()` og `replaceLast()`, som gir deg større kontroll over erstatningen.

## Se også

- [Kotlin Strings](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Regex i Kotlin](https://kotlinlang.org/docs/reference/regular-expressions.html)