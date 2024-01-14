---
title:    "Kotlin: Skrive tester"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/writing-tests.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive tester er en viktig del av programmering i Kotlin. Tester hjelper deg med å sikre at koden din fungerer som den skal, og reduserer risikoen for feil i produksjon. Det kan også hjelpe deg med å finne og løse problemer raskere, noe som gjør utviklingsprosessen mer effektiv.

## Hvordan

For å skrive tester i Kotlin, kan du bruke rammeverket JUnit. Dette rammeverket gjør det enkelt å lage tester ved å gi deg tilgang til en rekke funksjoner som hjelper deg med å sjekke data og forventet oppførsel.

For å skrive en enkel test, starter du med å importere JUnit rammeverket ved å legge til denne linjen øverst i filen:

```Kotlin
import org.junit.Test
```

Deretter kan du skrive en testfunksjon ved å bruke ```@Test```-anmerkningen og navngi den etterfulgt av parenteser:

```Kotlin
@Test
fun minTest() {
    // Testkode her
}
```

Inne i testfunksjonen kan du kjøre koden din og sjekke resultatene mot det du forventer ved hjelp av JUnit's assert-funksjoner. For eksempel:

```Kotlin
@Test
fun minTest() {
    // Initialiser data
    val a = 5
    val b = 10

    // Kjør koden din og få resultatet
    val resultat = a + b

    // Sjekk om resultatet er som forventet
    assertEquals(15, resultat)
}
```

Her bruker vi ```assertEquals()``` funksjonen for å sjekke om verdien av ```resultat``` er lik 15.

Det er også mulig å sette opp flere tester i samme fil, som hver har sine egne testfunksjoner og kan kjøres individuelt.

## Deep Dive

For å skrive mer komplekse tester, kan det være nyttig å benytte seg av JUnit's annotations. Disse kan hjelpe deg med å sette opp testmiljøer og kjøre spesifikke tester før og etter andre tester kjøres.

En annen viktig del av å skrive tester er å sørge for god dekning av koden din. Dette betyr at alle deler av koden din bør testes for å sikre at det ikke er noen skjulte feil. Du kan bruke dekningstester, som for eksempel jacoco, for å sjekke hvor mye av koden din som blir dekket av testene.

Å skrive effektive tester er en viktig ferdighet for å bli en bedre Kotlin-programmerer. Det kan også hjelpe deg med å forbedre kvaliteten og robustheten til koden din.

## Se Også

- [Offisiell JUnit dokumentasjon](https://junit.org/junit5/docs/current/user-guide/)
- [Dekningstesting med jacoco i Kotlin](https://www.baeldung.com/jacoco)
- [Eksempler på testing i Kotlin](https://www.baeldung.com/kotlin/testing)