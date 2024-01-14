---
title:    "C: Skriver tester"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/c/writing-tests.md"
---

{{< edit_this_page >}}

## Hvorfor

Når du jobber med å skrive kode, er det alltid viktig å forsikre deg om at koden din fungerer som den skal. Dette er spesielt viktig når du jobber på større og mer komplekse prosjekter. Ved å skrive tester for koden din, kan du oppdage og fikse eventuelle feil før de blir et større problem. Dette vil spare deg for mye tid og frustrasjon på lang sikt.

## Hvordan

Å skrive tester i C kan virke skremmende til å begynne med, men det er egentlig ikke så komplisert. La oss se på et eksempel: 

```C
#include <stdio.h>

int addTwo(int a, int b) {
  return a + b;
}

int main() {
  int result = addTwo(4, 3);
  printf("Resultatet er: %d", result);
  return 0;
}
```

Her har vi en enkel funksjon som legger sammen to tall og en main-funksjon som kaller på denne funksjonen og skriver ut resultatet. For å teste denne koden, kan vi skrive en enkel testfunksjon som ser slik ut:

```C
void test(int result) {
  if (result == 7) {
    printf("Testen besto!");
  } else {
    printf("Testen feilet!");
  }
}
```

Vi kaller deretter på denne testfunksjonen i main-funksjonen vår, rett etter at vi har beregnet resultatet. Slik ser koden vår ut nå:

```C
#include <stdio.h>

int addTwo(int a, int b) {
  return a + b;
}

void test(int result) {
  if (result == 7) {
    printf("Testen besto!");
  } else {
    printf("Testen feilet!");
  }
}

int main() {
  int result = addTwo(4, 3);
  test(result);
  printf("Resultatet er: %d", result);
  return 0;
}
```

Når vi nå kjører programmet vårt, vil vi få følgende resultat:

```Shell
Testen besto!
Resultatet er: 7
```

Vi ser at testen vår blir kalt på og at programmet vårt fungerer som det skal. Dette er et enkelt eksempel, men prinsippet gjelder for alle typer koding og kan tilpasses til ulike situasjoner.

## Dypdykk

Når du skriver tester, er det viktig å huske på at de bør være så dekkende som mulig. Det betyr at du bør teste både positive og negative scenarioer for å forsikre deg om at koden din håndterer alle mulige situasjoner. Det kan også være lurt å skrive tester før du skriver koden, slik at du kan bruke testene som en veiledning i utviklingsprosessen.

## Se også

- [En grundig guide til enhetstesting i C](https://www.jeremymorgan.com/tutorials/c-programming/how-to-unit-test-in-c/)
- [Tips for testing av C-kode](https://www.perforce.com/blog/qac/tips-testing-your-c-code)
- [Hvordan planlegge effektive tester i C](https://mousebee.net/how-to-plan-an-effective-c-testing-strategy)