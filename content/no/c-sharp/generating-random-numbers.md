---
title:                "Generering av tilfeldige tall"
html_title:           "C#: Generering av tilfeldige tall"
simple_title:         "Generering av tilfeldige tall"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor
Å generere tilfeldige tall er en viktig del av mange programmeringsoppgaver. Enten det er for å lage spill med tilfeldige elementer, teste algoritmer eller sikre sensitiv informasjon, er tilfeldige tall avgjørende for å skape variasjon og tilfeldighet i koden din.

## Hvordan
Generering av tilfeldige tall i C # er enkelt og kan gjøres på flere måter. Her er noen eksempler på hvordan du kan gjøre det:

```C#
// Generer et tilfeldig heltall mellom 1 og 10
Random rand = new Random();
int number = rand.Next(1, 11);
// Output: En tilfeldig verdi mellom 1 og 10

// Generer et tilfeldig desimaltall mellom 0 og 1
double decimalNumber = rand.NextDouble();
// Output: En tilfeldig verdi mellom 0 og 1

// Generer et tilfeldig heltall basert på en gitt liste av verdier
int[] numbers = {2, 4, 6, 8, 10};
int randomIndex = rand.Next(numbers.Length);
int chosenNumber = numbers[randomIndex];
// Output: En tilfeldig verdi fra listen av tall

```

Det er også mulig å konfigurere Random-objektet for å generere tilfeldige tall basert på en bestemt "frøverdi". Dette kan være nyttig hvis du vil kunne gjenskape de samme tilfeldige tallene i fremtidige kjøringer av koden din.

## Deep Dive
I bakgrunnen genererer Random-klassen en sekvens av tall basert på en matematisk formel. Disse tallene blir deretter omgjort til et heltall eller et flyttall avhengig av metoden som brukes (Next eller NextDouble). Hvis du ønsker å kunne generere tilfeldige tall av høyere kvalitet, kan du se nærmere på Random-klassen i .NET-rammeverket og dens forskjellige metoder og egenskaper.

## Se også
- [Microsoft dokumentasjon for Random-klassen](https://docs.microsoft.com/en-us/dotnet/api/system.random)
- [Tilfeldige tall i C#: En enkel guide](https://www.c-sharpcorner.com/article/random-number-generator-in-C-Sharp/)