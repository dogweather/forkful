---
date: 2024-01-26 04:37:58.312217-07:00
description: "Jak to zrobi\u0107: C++ posiada wbudowan\u0105 bibliotek\u0119 `<complex>`,\
  \ kt\xF3ra u\u0142atwia prac\u0119 z liczbami zespolonymi. Oto szybki przegl\u0105\
  d."
lastmod: '2024-03-13T22:44:35.706861-06:00'
model: gpt-4-0125-preview
summary: "C++ posiada wbudowan\u0105 bibliotek\u0119 `<complex>`, kt\xF3ra u\u0142\
  atwia prac\u0119 z liczbami zespolonymi."
title: Praca z liczbami zespolonymi
weight: 14
---

## Jak to zrobić:
C++ posiada wbudowaną bibliotekę `<complex>`, która ułatwia pracę z liczbami zespolonymi. Oto szybki przegląd:

```cpp
#include <iostream>
#include <complex>

int main() {
    std::complex<double> num1(2.0, 3.0); // Tworzy liczbę zespoloną (2 + 3i)
    std::complex<double> num2(3.0, 4.0); // Kolejna liczba zespolona (3 + 4i)

    // Dodawanie
    std::complex<double> result = num1 + num2;
    std::cout << "Wynik dodawania: " << result << std::endl; // (5 + 7i)

    // Mnożenie
    result = num1 * num2;
    std::cout << "Wynik mnożenia: " << result << std::endl; // (-6 + 17i)

    // Sprzężenie
    result = std::conj(num1);
    std::cout << "Sprzężenie num1: " << result << std::endl; // (2 - 3i)
    
    return 0;
}
```

## Pogłębiona analiza
Liczby zespolone mają bogatą historię, pojawiając się po raz pierwszy w rozwiązaniach równań sześciennych w XVI wieku. Są niezbędne w wielu dziedzinach, nie tylko w programowaniu. W informatyce, liczby zespolone pomagają w algorytmach wymagających dwuwymiarowej przestrzeni liczbowej, takich jak Szybka Transformata Fouriera (FFT).

Chociaż biblioteka `<complex>` w C++ jest standardem, alternatywy istnieją w innych językach, takich jak typ danych `complex` w Pythonie czy biblioteki matematyczne w JavaScript. Samo `<complex>` oferuje wszechstronną funkcjonalność, w tym operacje trygonometryczne, wykładnicze i logarytmiczne dostosowane do liczb zespolonych.

Podczas programowania tych liczb kluczowe jest zrozumienie leżącej u ich podstaw matematyki, aby zapobiec nieścisłościom i zrozumieć operacje takie jak sprzężenie zespolone, które zmienia znak części urojonej, czy implikacje wzoru Eulera wiążącego eksponencje zespolone z funkcjami trygonometrycznymi.

## Zobacz również
- Dokumentacja Standardowej Biblioteki Szablonów C++: https://en.cppreference.com/w/cpp/header/complex
- Głębsze zanurzenie matematyczne w liczby zespolone: https://mathworld.wolfram.com/ComplexNumber.html
- Do wizualizacji biblioteka Pythona Matplotlib może wykreślać liczby zespolone: https://matplotlib.org/
