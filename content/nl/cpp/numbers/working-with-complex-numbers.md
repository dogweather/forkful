---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:20.862198-07:00
description: 'Hoe: C++ heeft een ingebouwde bibliotheek `<complex>` die het werken
  met complexe getallen vergemakkelijkt. Hier is een snelle blik.'
lastmod: '2024-03-13T22:44:51.106587-06:00'
model: gpt-4-0125-preview
summary: C++ heeft een ingebouwde bibliotheek `<complex>` die het werken met complexe
  getallen vergemakkelijkt.
title: Werken met complexe getallen
weight: 14
---

## Hoe:
C++ heeft een ingebouwde bibliotheek `<complex>` die het werken met complexe getallen vergemakkelijkt. Hier is een snelle blik:

```cpp
#include <iostream>
#include <complex>

int main() {
    std::complex<double> num1(2.0, 3.0); // Creëert een complex getal (2 + 3i)
    std::complex<double> num2(3.0, 4.0); // Nog een complex getal (3 + 4i)

    // Optelling
    std::complex<double> resultaat = num1 + num2;
    std::cout << "Resultaat van optelling: " << resultaat << std::endl; // (5 + 7i)

    // Vermenigvuldiging
    resultaat = num1 * num2;
    std::cout << "Resultaat van vermenigvuldiging: " << resultaat << std::endl; // (-6 + 17i)

    // Geconjugeerde
    resultaat = std::conj(num1);
    std::cout << "Geconjugeerde van num1: " << resultaat << std::endl; // (2 - 3i)
    
    return 0;
}
```

## Diepgaande duik
Complexe getallen hebben een rijke geschiedenis, voor het eerst opduikend in oplossingen voor kubieke vergelijkingen in de 16e eeuw. Ze zijn essentieel in vele velden, niet alleen in programmering. Binnen de informatica helpen complexe getallen bij algoritmen die een tweedimensionale getallenruimte vereisen, zoals de Fast Fourier Transform (FFT).

Hoewel de `<complex>` bibliotheek van C++ standaard is, bestaan er in andere talen alternatieven, zoals het `complex` gegevenstype in Python of de wiskundige bibliotheken in JavaScript. De `<complex>` bibliotheek zelf biedt uitgebreide functionaliteit, inclusief trigonometrische, exponentiële en logaritmische operaties op maat van complexe getallen.

Bij het programmeren van deze getallen is het cruciaal om de onderliggende wiskunde te begrijpen om onnauwkeurigheden te voorkomen en operaties zoals complexe conjugatie te begrijpen, wat het teken van het imaginaire deel omkeert, of de implicaties van de formule van Euler die complexe exponentiëlen relateert aan trigonometrische functies.

## Zie ook
- De C++ Standaard Template Bibliotheek Documentatie: https://en.cppreference.com/w/cpp/header/complex
- Een diepere wiskundige duik in complexe getallen: https://mathworld.wolfram.com/ComplexNumber.html
- Voor visualisatie, kan de Python-bibliotheek Matplotlib complexe getallen plotten: https://matplotlib.org/
