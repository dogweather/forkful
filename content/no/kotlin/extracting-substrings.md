---
title:                "Utdrag av understrenger"
html_title:           "Bash: Utdrag av understrenger"
simple_title:         "Utdrag av understrenger"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?

Å trekke ut substrings betyr å velge deler av en streng basert på posisjon eller mønster. Dette er viktig i programmering for å manipulere tekstdata effektivt.

## Hvordan:

Her er noen enkle eksempler på hvordan du trekker ut substrings i Kotlin:

```Kotlin
val text = "Hei,Verden!"
println(text.substring(0,3)) //Output: Hei

val fraIndex = text.indexOf(",") + 1
println(text.substring(fraIndex)) //Output: Verden!
```

I det første eksemplet trekker vi ut de første tre tegnene av strengen. I det andre eksempelet finner vi indexen til kommaet og trekker ut alt som kommer etter.

## Dypdykk:

Hensikten med å trekke ut substrings går langt tilbake i programmeringshistorien. Tekstbehandling og mønster-gjenkjenning er noen av hjørnesteinene i informatikk. 

I Kotlin kan du også bruke `substringBefore` og `substringAfter` funksjonene som gir deg mer direkte kontroll over hva som ekstraheres.

```Kotlin
val text = "Hei,Verden!"
println(text.substringBefore(",")) //Output: Hei
println(text.substringAfter(",")) //Output: Verden!
```

En annen viktig detalj er håndteringen av 'IndexOutOfBoundsException'. Dersom du prøver å trekke ut en substring utenfor strengens lengde, vil programmet kaste en unntak. Det er derfor viktig å sjekke indeksene før man prøver å trekke ut substrings.

## Se også:

1. [Offisiell Kotlin dokumentasjon på substrings](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/substring.html)