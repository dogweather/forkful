---
title:                "C: Skrive tester"
programming_language: "C"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/writing-tests.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive tester for koden din kan virke som en ekstra byrde, men det er en viktig del av å utvikle kvalitetsprogrammer. Tester hjelper deg med å identifisere og fikse feil tidlig i utviklingsprosessen, noe som sparer deg for mye tid og stress på lang sikt. Så hvorfor skal man engasjere seg i å skrive tester? La oss se nærmere på det!

## Hvordan

For å skrive tester må du først ha et godt utgangspunkt - en fungerende kode. La oss ta et enkelt eksempel på en funksjon for å regne ut fakultetet til et tall:

```C
#include <stdio.h>

// Funksjon for å regne ut fakultetet til et tall
int factorial(int n)
{
    int result = 1;
    for (int i = 1; i <= n; i++)
    {
        result *= i;
    }
    return result;
}

// Hovedfunksjon
int main()
{
    int n = 5;
    int answer = factorial(n);
    printf("Fakultetet av %d er %d\n", n, answer);
    return 0;
}
```

Når vi kjører koden vår, får vi følgende output:

```
Fakultetet av 5 er 120
```

Nå som vi vet at koden vår fungerer, kan vi begynne å skrive tester for den. For å gjøre dette, må vi bruke et testrammeverk som for eksempel [CUnit](http://cunit.sourceforge.net/). Her er et eksempel på en enkel test som sjekker om funksjonen vår returnerer riktig svar når vi gir den tallet 4 som argument:

```C
#include <CUnit/CUnit.h>
#include <CUnit/Basic.h>

// Testfunksjon
void testFactorial()
{
    CU_ASSERT_EQUAL(factorial(4), 24); // Sjekker om svaret er 24
}

// Hovedfunksjon
int main()
{
    // Initialiserer testmiljøet
    CU_initialize_registry();

    // Oppretter en ny test-suite
    CU_pSuite suite = CU_add_suite("factorial_test_suite", NULL, NULL);

    // Legger til testfunksjonen vår i test-suiten
    CU_add_test(suite, "testFactorial", testFactorial);

    // Kjører testene
    CU_basic_run_tests();

    // Rydder opp etter at testene er kjørt
    CU_cleanup_registry();

    return 0;
}
```

Etter å ha kjørt testene, vil vi få følgende output:

```
CUnit - A Unit testing framework for C - Version 2.1-3
http://cunit.sourceforge.net/

Suite: factorial_test_suite
  Test: testFactorial ...passed

Run Summary:    Type  Total    Ran Passed Failed Inactive
              suites      1      1    n/a      0        0
               tests      1      1      1      0        0
             asserts      1      1      1      0      n/a

Elapsed time =    0.000 seconds
```

Vi kan se at testen vår passerte, noe som betyr at funksjonen vår fungerer som den skal.

## Dypdykk

Å skrive tester kan også hjelpe deg med å forbedre koden din. Mens du skriver tester, må du tenke på alle mulige tilfeller som kan påvirke funksjonen din. Dette kan føre til at du skriver mer robust og feilsikker kode. I tillegg hjelper tester deg med å dokumentere koden din, siden du må gi klare og konsise forklaringer på hva hver test utfører.

I tillegg til enhetstesting, kan det også være nyttig å implementere integrasjonstester og funksjonelle tester for å sikre at alle deler av koden din fungerer godt sammen og utfører de ønskede oppgavene.

## Se også

- [CUnit dokumentasjon](http://cunit.sourceforge.net/doc/index.html)
- [Introduksjon til enhetstesting i C](https://www.ibm.com/developerworks/rational/library/introduction-to-unit-testing-in-c-cplusplus