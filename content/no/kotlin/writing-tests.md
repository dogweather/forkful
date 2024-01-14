---
title:    "Kotlin: Skrive tester"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Hvorfor

Å skrive tester er en viktig del av å skrive god og pålitelig kode. Tester hjelper til med å oppdage eventuelle feil eller bugs i koden tidligere, noe som gjør det enklere og raskere å fikse dem. Testdrevet utvikling (TDD) er også en populær og anbefalt tilnærming til å skrive kode, og det krever at man skriver tester før man skriver selve koden.

## Hvordan

For å skrive tester i Kotlin kan man bruke et test-rammeverk som for eksempel JUnit eller Mockito. Et eksempel på en enkel test i Kotlin med JUnit ser slik ut:

```Kotlin
import org.junit.Test
import org.junit.Assert.*

class StringManipulationTest {

    @Test
    fun reverseString_returnsReversedString() {
        val originalString = "Hello"
        val reversedString = originalString.reverse()
        assertEquals("olleH", reversedString)
    }
}
```

Her blir det laget en testklasse for en funksjon som skal reversere en streng. I testen blir det laget en original streng og en forventet reversert streng. Deretter blir funksjonen kalt og resultatet blir sammenlignet med den forventede verdien ved hjelp av `assertEquals` metoden fra JUnit. Om testen feiler vil man få en feilmelding som vil hjelpe en med å finne og fikse feilen.

## Dypdykk

Når man skriver tester er det viktig å ha et godt og representativt utvalg av tester. Dette betyr at man bør teste både positive og negative scenarier, samt ulike inndata for å sikre at funksjonen oppfører seg som forventet. I tillegg kan man også bruke mock-objekter for å teste funksjoner som er avhengige av eksterne komponenter eller tjenester.

Man bør også prøve å holde testene så uavhengige som mulig. Dette betyr at testene ikke bør være avhengige av hverandre og de bør kunne kjøres i hvilken som helst rekkefølge. Dette gjør det enklere å identifisere og fikse eventuelle feil.

## Se også

- [Kotlin test-rammeverk](https://kotlinlang.org/docs/tutorials/jvm-get-started.html#setting-up )
- [Testdrevet utvikling med Kotlin](https://vividcode.io/kotlin-test-driven-development/)
- [JUnit dokumentasjon](https://junit.org/junit5/docs/current/user-guide/)