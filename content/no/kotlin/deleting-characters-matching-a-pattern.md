---
title:                "Slette tegn som samsvarer med et mønster"
html_title:           "Kotlin: Slette tegn som samsvarer med et mønster"
simple_title:         "Slette tegn som samsvarer med et mønster"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Hvorfor
Noen ganger kan du finne deg selv i en situasjon der du må fjerne visse tegn i en tekststreng som matcher et mønster. Dette kan være for å rense data, gjøre en tekst mer lesbar eller for å oppnå et bestemt format. Ved å lære hvordan du sletter tegn som matcher et mønster i Kotlin, kan du enkelt håndtere slike situasjoner og øke effektiviteten din som utvikler.

# Slik gjør du det
For å slette tegn som matcher et mønster i Kotlin, kan du bruke funksjonene `replace()` og `replaceAll()`. Disse funksjonene tar inn to parametere: et regulært uttrykk og en streng som representerer teksten du ønsker å endre.

```Kotlin
val text = "Hei! Jeg elsker å kode i Kotlin :)"

// Fjerner alle tegn som er ikke-bokstaver
val nyTekst = text.replace("[^a-zA-Z]".toRegex(), "")
println(nyTekst) // Resultat: HeiJegelskeråkodeiKotlin

// Fjerner spesifikke tegn som matcher et mønster
val annenTekst = text.replaceAll("[!:]".toRegex(), "")
println(annenTekst) // Resultat: Hei Jeg elsker å kode i Kotlin
```

I det første eksempelet bruker vi `replace()` for å fjerne alle tegn som ikke er bokstaver fra teksten vår. I det andre eksempelet bruker vi `replaceAll()` for å fjerne utropstegn og kolon fra teksten. Det regulære uttrykket som blir brukt, forteller funksjonen hvilke tegn den skal erstatte.

# Dypdykk
I de to eksemplene over har vi brukt regulære uttrykk for å spesifisere hvilke tegn vi ønsker å fjerne. Regulære uttrykk er en kraftig måte å søke og manipulere tekst på, og det er derfor verdt å bruke litt tid på å forstå de forskjellige mønstrene og symbolene som kan brukes.

Noen vanlige symboler som brukes i regulære uttrykk er:

- `.` - matcher et hvilket som helst tegn
- `[a-z]` - matcher et hvilket som helst tegn fra a til z
- `^` - matcher starten av en tekststreng
- `$` - matcher slutten av en tekststreng
- `*` - matcher null eller flere forekomster av det forrige tegnet
- `+` - matcher én eller flere forekomster av det forrige tegnet
- `?` - matcher null eller én forekomst av det forrige tegnet

I tillegg til disse er det mange andre symboler og mønstre som kan brukes for å lage mer avanserte regulære uttrykk. Det kan være lurt å øve seg på å skrive og teste ulike uttrykk for å bli mer komfortabel med dem.

# Se også
- Dokumentasjon for `replace()` og `replaceAll()` i Kotlin: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/replace.html
- En oversikt over vanlige regulære uttrykk: https://www.rexegg.com/regex-quickstart.html