---
title:    "C: Skriving av tester"
keywords: ["C"]
---

{{< edit_this_page >}}

## Hvorfor
Å skrive tester er en viktig del av å være en effektiv C-programmerer. Tester hjelper deg med å identifisere feil og sikre at koden din fungerer som forventet. Det sparer deg for tid og frustrasjon i det lange løp.

## Hvordan
For å skrive tester i C, kan du bruke et testrammeverk som for eksempel Check eller CUnit. Her er et eksempel på en enkel testfunksjon som sjekker om to tall er like:

```C
#include <stdio.h>
#include "check.h"

START_TEST(test_equal_numbers) {
    int a = 5;
    int b = 5;
    ck_assert_int_eq(a, b);
}
END_TEST

int main() {
    Suite *s = suite_create("My Suite");

    TCase *tc_case = tcase_create("Equal Numbers Test");
    tcase_add_test(tc_case, test_equal_numbers);
    suite_add_tcase(s, tc_case);

    SRunner *sr = srunner_create(s);
    srunner_run_all(sr, CK_NORMAL);
    int failed = srunner_ntests_failed(sr);
    srunner_free(sr);
    return (failed == 0) ? 0 : 1;
}
```

Her har vi brukt Check-testrammeverket og definert en testfunksjon ved hjelp av `START_TEST` og `END_TEST` makroer. Inkluder check.h for å få tilgang til disse makroene. Deretter oppretter vi en test-suite og en test-case, og legger til testfunksjonen i test-case. Til slutt kjører vi alt ved hjelp av `srunner_run_all` og sjekker om det er noen feil. For å kjøre testene må vi også inkludere `-lcheck` flagg når vi kompilerer.

## Dypdykk
Å skrive gode tester er en kunst, og det er noen ting du bør vurdere når du skriver tester. Her er noen tips:

- Skriv tester som reflekterer brukerens forventninger til koden din
- Sørg for å ha god testdekning og test alle mulige scenarioer
- Hold testene dine oppdatert og legg til nye tester når du legger til ny funksjonalitet i koden din

## Se også
- [Check-testrammeverket](https://libcheck.github.io/check/)
- [CUnit-testrammeverket](https://github.com/mikesteele81/cunit)
- [Generell informasjon om testing i C](https://raymii.org/s/tutorials/Cpp_project_setup_with_cmake_and_unit_tests.html)