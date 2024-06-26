---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:24.068895-07:00
description: "Jak to zrobi\u0107: W C++, tablice asocjacyjne o\u017Cywaj\u0105 dzi\u0119\
  ki nag\u0142\xF3wkom `<map>` i `<unordered_map>`. Przyjrzyjmy si\u0119 przyk\u0142\
  adom, aby zobaczy\u0107 oba w akcji."
lastmod: '2024-04-05T21:53:37.132092-06:00'
model: gpt-4-0125-preview
summary: "W C++, tablice asocjacyjne o\u017Cywaj\u0105 dzi\u0119ki nag\u0142\xF3wkom\
  \ `<map>` i `<unordered_map>`."
title: Korzystanie z tablic asocjacyjnych
weight: 15
---

## Jak to zrobić:
W C++, tablice asocjacyjne ożywają dzięki nagłówkom `<map>` i `<unordered_map>`. Przyjrzyjmy się przykładom, aby zobaczyć oba w akcji.

### Używanie `std::map`
`std::map` przechowuje elementy posortowane na podstawie klucza. Oto jak zacząć:

```C++
#include <iostream>
#include <map>
#include <string>

int main() {
    std::map<std::string, int> ageMap;
    
    // Wstawianie wartości
    ageMap["Alice"] = 30;
    ageMap["Bob"] = 25;
    
    // Dostęp do wartości
    std::cout << "Wiek Boba: " << ageMap["Bob"] << std::endl;
    
    // Iteracja po mapie
    for(const auto &pair : ageMap) {
        std::cout << pair.first << " ma " << pair.second << " lat." << std::endl;
    }
    
    return 0;
}
```

### Używanie `std::unordered_map`
Kiedy kolejność nie ma znaczenia, ale ma znaczenie wydajność, `std::unordered_map` jest twoim przyjacielem, oferując szybszą średnią złożoność dla wstawień, wyszukiwań i usunięć.

```C++
#include <iostream>
#include <unordered_map>
#include <string>

int main() {
    std::unordered_map<std::string, double> productPrice;
    
    // Wstawianie wartości
    productPrice["mleko"] = 2.99;
    productPrice["chleb"] = 1.99;
    
    // Dostęp do wartości
    std::cout << "Cena mleka: $" << productPrice["mleko"] << std::endl;
    
    // Iteracja po unordered_map
    for(const auto &pair : productPrice) {
        std::cout << pair.first << " kosztuje $" << pair.second << std::endl;
    }
    
    return 0;
}
```

## Pogłębienie wiedzy
Tablice asocjacyjne w C++, zwłaszcza `std::map` i `std::unordered_map`, to nie tylko przechowywanie elementów. Zapewniają one podstawę do bardziej skomplikowanego zarządzania danymi, pozwalając na operacje takie jak wyszukiwanie, wstawianie i usuwanie w efektywnych złożonościach czasowych (logarytmiczna dla `std::map` i średnio stała dla `std::unordered_map`). Ta efektywność wynika z leżących u ich podstaw struktur danych: zrównoważonego drzewa dla `std::map` i tablicy mieszającej dla `std::unordered_map`.

Historycznie, zanim te stały się częścią standardowej biblioteki, programiści musieli implementować własne wersje lub używać bibliotek stron trzecich, prowadząc do niespójności i potencjalnych nieefektywności. Włączenie map do standardowej biblioteki C++ nie tylko ujednoliciło ich użycie, ale także zoptymalizowało je pod kątem wydajności na różnych kompilatorach i platformach.

Chociaż oba są potężne, wybór między `std::map` a `std::unordered_map` zależy od szczegółów konkretnego przypadku użycia. Potrzebujesz uporządkowanych danych i nie przeszkadza ci nieco gorsza wydajność? Wybierz `std::map`. Jeśli zależy ci na szybkości i kolejność nie ma znaczenia, `std::unordered_map` prawdopodobnie będzie lepszym wyborem.

Jednakże ważne jest, by pamiętać, że przy pracy z złożonymi strukturami danych zawsze istnieją kompromisy. W niektórych niszowych przypadkach inne struktury danych lub nawet biblioteki stron trzecich mogą oferować lepszą wydajność lub funkcjonalność dostosowaną do twoich szczególnych potrzeb. Zawsze waż swoje opcje na podstawie wymagań twojego projektu.
