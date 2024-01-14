---
title:                "Arduino: Skrive tester"
simple_title:         "Skrive tester"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/writing-tests.md"
---

{{< edit_this_page >}}

## Hvorfor
Det å skrive tester er en viktig del av arbeidsflyten til enhver programmerer. Ved å skrive tester sikrer man kvalitet og pålitelighet i koden sin, og det blir enklere å finne og rette feil senere.

## Hvordan
For å skrive tester i Arduino er det viktig å først definere hva man ønsker å teste. Deretter kan man bruke assert-syntaksen for å sjekke om resultatet av testene er som forventet. Her er et eksempel på hvordan man kan skrive en enkel test for å sjekke om to tall er like:

```Arduino
int tall1 = 5;
int tall2 = 5;

assert(tall1 == tall2); // Sjekker om tall1 er lik tall2
```

Dette er en enkel måte å starte med testing på, men det finnes mange forskjellige metoder og syntakser man kan bruke basert på hva man ønsker å teste.

## Dypdykk
Når man begynner å bli komfortabel med å skrive tester i Arduino, er det viktig å også forstå hvordan man kan lage mer avanserte tester. Dette kan inkludere å bruke biblioteker som gir mer funksjonalitet til testing, eller å bruke objektorientert programmering for å strukturere og organisere testene på en bedre måte.

Det er også viktig å huske at testing ikke bare handler om å skrive tester, men også om å skrive god og pålitelig kode. Ved å følge god programmeringspraksis og bruke tester som en del av utviklingsprosessen, kan man sikre seg at koden fungerer som den skal og er lett å vedlikeholde.

## Se også
- [Offisiell Arduino-veiledning for testing](https://www.arduino.cc/en/Guide/UnitTesting)
- [Eksempel på bruk av assert-syntaksen i Arduino](https://www.arduino.cc/reference/en/language/functions/debugging/assert/)
- [Artikkel om betydningen av testing i programmering](https://medium.com/@ericelliott/javascript-fatigue-end-to-end-testing-7e245e478e42)