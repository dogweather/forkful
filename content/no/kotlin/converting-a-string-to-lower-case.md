---
title:                "Konvertering av streng til små bokstaver"
html_title:           "Kotlin: Konvertering av streng til små bokstaver"
simple_title:         "Konvertering av streng til små bokstaver"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Konvertering av en streng til små bokstaver refererer til å endre alle bokstaver til små, ikke-uthevede versjoner av seg selv. Dette er en vanlig oppgave for programmerere å gjøre når de trenger å behandle tekst som ikke er tilgjengelig i det det øyeblikket.

## Slik gjør du:
```Kotlin 
var uppercaseStr = "HELLO WORLD" 
var lowercaseStr = uppercaseStr.toLowerCase() 
println(lowercaseStr)
```
Dette kodeeksempelet vil endre verdien av `uppercaseStr` til `hello world` og skrive ut den nye verdien. 

## Dykk dypere:
Historisk sett har konvertering av strenger til små bokstaver vært en vanlig oppgave i programmering. Alternativt kan man også bruke `toUpperCase()` for å endre alle bokstaver til store bokstaver. Implementasjonsdetaljer varierer avhengig av programmeringsspråk, men det finnes vanligvis dedikerte funksjoner eller metoder for å konvertere strenger til både små og store bokstaver.

## Se også:
For mer informasjon og eksempler på hvordan konvertere strenger til små bokstaver i Kotlin, sjekk ut Kotlin dokumentasjonen på https://kotlinlang.org/docs/reference/basic-types.html#strings