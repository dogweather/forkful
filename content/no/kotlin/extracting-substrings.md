---
title:                "Uthenting av delstrenger"
date:                  2024-01-20T17:46:11.625885-07:00
model:                 gpt-4-1106-preview
simple_title:         "Uthenting av delstrenger"

category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?
Hva er ekstrahering av delstrenger? Det er når du plukker ut deler av en tekststreng. Hvorfor? Fordi noen ganger trenger du bare et spesifikt stykke av informasjonen, som brukernavn fra en e-post eller en dato fra en tekst.

## How to:
```Kotlin
fun main() {
    val fullString = "hello@world.com"
    val userName = fullString.substringBefore("@")
    val domain = fullString.substringAfter("@")

    println(userName)  // Skriver ut: hello
    println(domain)   // Skriver ut: world.com

    val date = "2023-04-01"
    val year = date.substring(0, 4)
    val month = date.substring(5, 7)
    
    println(year)  // Skriver ut: 2023
    println(month) // Skriver ut: 04
}
```

## Deep Dive
Delstrengekstraksjon går tilbake til de tidlige dagene av programmering. Språk som C brukte funksjoner som `substr`. I Kotlin, er det innebygde funksjoner som `substring`, `substringBefore`, og `substringAfter`. Disse metodene støtter både enkelhet og lesbarhet.

Når det gjelder alternativer, kan du bruke regulære uttrykk for komplekse mønstre. Men for enkelhetens skyld, er `substring`-funksjonene ofte å foretrekke. Kotlin håndterer `String`-referanser ved å bruke immutabilitet og utfører subsekvent kopiering for å sikre ytelse og unngå endringer i den opprinnelige strengen.

## See Also
- [Regex i Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/index.html)
