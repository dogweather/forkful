---
title:                "Pisanie testów"
html_title:           "C: Pisanie testów"
simple_title:         "Pisanie testów"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/writing-tests.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie testów jest ważną i nieodłączną częścią procesu tworzenia oprogramowania. Testy pomagają upewnić się, że nasz kod działa zgodnie z oczekiwaniami oraz zapobiegają wprowadzaniu błędów podczas dalszego rozwoju projektu.

## Jak To Zrobić

Pisanie testów w języku C jest prostsze, niż się wydaje. Wystarczy skorzystać z dostępnych bibliotek, takich jak "CUnit" lub "Check", aby utworzyć zestaw testów dla naszego kodu. Poniżej znajduje się przykładowy kod oraz jego wyjście, które pozwolą lepiej zrozumieć proces tworzenia testów.

```C
#include <stdlib.h>
#include <stdio.h>
#include <CUnit/CUnit.h>
#include <CUnit/Basic.h>

//Funkcja, którą będziemy testować
int dodaj(int a, int b){
    return a + b;
}

//Funkcja inicjująca testy
int init_suite(void) {
   return 0;
}

//Funkcja kończąca testy
int clean_suite(void) {
    return 0;
}

//Test sprawdzający poprawność działania funkcji dodawania
void test_dodawania(void) {
    CU_ASSERT_EQUAL(dodaj(2, 3), 5);
    CU_ASSERT_EQUAL(dodaj(0, -5), -5);
}

//Dodanie testów do zestawu
int main() {
    CU_pSuite pSuite = NULL;
    
    if (CU_initialize_registry() != CUE_SUCCESS) {
        return CU_get_error();
    }

    pSuite = CU_add_suite("Suite_testowa", init_suite, clean_suite);
    if (NULL == pSuite) {
        CU_cleanup_registry();
        return CU_get_error();
    }

    //Dodanie testu do zestawu
    if (NULL == CU_add_test(pSuite, "test_dodawania", test_dodawania)) {
        CU_cleanup_registry();
        return CU_get_error();
    }

    //Uruchomienie testów
    CU_basic_set_mode(CU_BRM_VERBOSE);
    CU_basic_run_tests();
    printf("\n");

    //Wynik testów
    CU_cleanup_registry();
    return CU_get_error();
}
```
**Wyjście:**
```
CUNIT: Suite: Suite_testowa
CUNIT: Test sprzedania
CUNIT: Teste dodania OK.
CUNIT: 1 testy z 1 zdanych.

KWADRA:  Suite: Suite_testowa
KWADRA: Test sauda
KWADRA: Test dodania OK.
KWADRA: 1000 testów z 1000 zdanych

Wynik: Świetnie! Twój kod działa zgodnie z oczekiwaniami!

## Głębszy Wgląd
Jedną z najważniejszych rzeczy przy pisaniu testów jest tworzenie testów jednostkowych, które testują pojedyncze funkcje naszego kodu. Testy jednostkowe powinny być napisane w taki sposób, aby pokryć wszystkie możliwe przypadki działania funkcji.

Warto również pamiętać, że testy powinny być uruchamiane regularnie, najlepiej przy każdej zmianie w kodzie. Dzięki temu unikniemy niespodzianek w postaci błędów, które mogą pojawić się na późniejszych etapach projektu.

Podczas pisania testów, warto również korzystać z asserts, które pomagają weryfikować wyjście naszych funkcji oraz CU_FAIL, który pomaga w łatwiejszym debugowaniu kodu.

## Zobacz także
1. [CUnit User's Guide](http://cunit.sourceforge.net/doc/index.html)
2. [Using Assertions in C](https://www.cs.umb.edu/~smimarog/simple_c.html)
3. [Effective Unit Testing in C](https://medium.com/level-up-programming/effective-unit-testing-in-c-b160c541348d)
4. [Check Unit Testing Framework](https://libcheck.github.io/check/doc/check_html/index.html)

**See Also** 
1. [Podręcznik użytkownika CUnit](http://