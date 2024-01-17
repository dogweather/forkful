---
title:                "Utskrift av feilsøkningsdata"
html_title:           "Kotlin: Utskrift av feilsøkningsdata"
simple_title:         "Utskrift av feilsøkningsdata"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

# Hva og hvorfor?
 
 "Printing debug output" er en måte for utviklere å se informasjon om hva som skjer i koden deres mens den kjører. Dette kan være nyttig for å feilsøke og finne feil i koden. Det er vanlig for programmerere å bruke "printing debug output" for å sikre at koden deres fungerer som den skal og for å forstå hva som skjer under kjøringen av programmet.

## Hvordan:

La oss si vi har en funksjon som skal legge sammen to tall og returnere resultatet. Vi ønsker å sjekke om funksjonen fungerer som den skal, så vi printer ut resultatet.

```Kotlin
fun addNumbers(num1: Int, num2: Int): Int {
    val result = num1 + num2
    println("The sum of $num1 and $num2 is $result")
    return result
}

addNumbers(5, 3)
```

Output:
```Kotlin
The sum of 5 and 3 is 8
```

Her kan vi se at funksjonen fungerer som den skal og vi får det forventede resultatet. Dette kan hjelpe oss å bekrefte at koden vår er riktig og at resultatet blir håndtert på riktig måte.

## Dypdykk:

Printing debug output har vært en vanlig måte å feilsøke koden siden de tidlige dagene av programmering. Andre metoder, som for eksempel å bruke en "debugger", har også blitt mer populære. En debugger lar utviklere gå gjennom koden sin linje for linje for å se hva som skjer mens programmet kjører. Men printing debug output kan fortsatt være en nyttig metode for rask feilsøking og for å få en generell forståelse av hvordan koden fungerer.

Når vi printer ut informasjon fra koden vår, er det viktig å være forsiktig så vi ikke printer ut sensitiv informasjon som kan være skadelig for programmet vårt. Det er også viktig å huske å fjerne printing debug output-koden før vi ferdigstiller programmet vårt, slik at det ikke påvirker ytelsen til programmet.

## Se også:

* [Kotlin Offisiell Dokumentasjon] (https://kotlinlang.org/docs/reference/basic-syntax.html#using-string-templates) for mer informasjon om printing debug output i Kotlin.
* [Debugging Techniques] (https://www.geeksforgeeks.org/debugging-techniques/) for å lære mer om ulike metoder for å feilsøke koden din.
* [Kotlin Playground] (https://play.kotlinlang.org/) for å prøve ut og eksperimentere med Kotlin-kode.