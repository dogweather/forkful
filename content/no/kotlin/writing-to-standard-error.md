---
title:                "Skriver til standardfeil"
html_title:           "Kotlin: Skriver til standardfeil"
simple_title:         "Skriver til standardfeil"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du noen gang har programmert, har du sannsynligvis sett uttrykket "skrive til standard feil" eller "skrive til konsollen". Dette betyr simpelthen å sende en beskjed eller en utskrift til konsollen mens koden kjører. Men hvorfor er dette nyttig?

Å skrive til standard feil (også kjent som standardfeilkonsoll) gir deg muligheten til å gi informasjon eller feilmeldinger om koden din mens den kjører. Dette er spesielt nyttig når du skal debugge og finne ut hvorfor koden din ikke fungerer som den skal.

## Slik gjør du det

For å skrive til standard feil i Kotlin, bruker du funksjonen "System.err.println()". Du kan plassere denne funksjonen hvor som helst i koden din for å sende en melding til konsollen.

```Kotlin

fun main() {
  System.err.println("Dette er en testmelding som vil bli skrevet til standard feil!")
}

// Output: Dette er en testmelding som vil bli skrevet til standard feil!
```

Hvis du vil kaste et unntak (exception) og skrive en feilmelding til konsollen, kan du bruke "System.err.println()" inne i et try-catch-blokk.

```Kotlin

fun main() {
  try {
    var x = 10 / 0
  } catch (e: Exception) {
    System.err.println("Kan ikke dele med 0. Feilmelding: " + e.message)
  }
}

// Output: Kan ikke dele med 0. Feilmelding: / by zero
```

## Dypdykk

Når du skriver til standard feil, kan du også styre utseendet på meldingene som blir sendt til konsollen. Dette kan være nyttig for å gjøre meldingene mer tydelige og organiserte.

Du kan bruke "System.err.print()" for å sende en melding uten linjeskift eller "System.err.printf()" for å formatere meldingene dine.

```Kotlin

fun main() {
  // Bruk System.err.print() for å sende meldinger uten linjeskift
  System.err.print("Dette er en ")
  System.err.print("testmelding. ")

  // Bruk System.err.printf() for å formatere meldinger
  val x = 10
  val y = 5
  System.err.printf("%d er større enn %d", x, y)

}

// Output: Dette er en testmelding. 10 er større enn 5
```

## Se også

- [Kotlin Dokumentasjon](https://kotlinlang.org/docs/home.html)
- [Hvordan skrive til standardfeil i Java](https://www.geeksforgeeks.org/system-out-println-in-java-with-examples/)
- [Feilbehandling i Kotlin](https://kotlinlang.org/docs/reference/exceptions.html)