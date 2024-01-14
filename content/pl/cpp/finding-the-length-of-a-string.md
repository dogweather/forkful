---
title:    "C++: Znajdowanie długości ciągu znaków"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego
Jednym z najważniejszych zadań programistów jest manipulacja i przetwarzanie danych. W przypadku pracy z tekstami, często musimy znać długość danego ciągu znaków. Właśnie dlatego znajomość sposobu obliczenia długości stringa jest niezbędna dla każdego programisty.

## Jak to zrobić
Obliczenie długości stringa w C++ jest bardzo proste. Wystarczy skorzystać z funkcji `length()` lub `size()`, które zwracają liczbę znaków w podanym stringu. Poniżej znajdują się przykłady kodu pokazujące wykorzystanie tych funkcji oraz ich wynik.

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    string tekst = "Cześć, to jest przykładowy tekst.";
    cout << "Długość tekstu to: " << tekst.length() << endl;
    cout << "Długość tekstu to: " << tekst.size() << endl;
    
    return 0;
}

/* Wynik:
Długość tekstu to: 31
Długość tekstu to: 31
*/
```

Podobnie jak wiele innych funkcji, `length()` i `size()` mogą być także wywoływane na obiektach typu `std::string`, dlatego też możemy skrócić nasz kod do bardziej czytelnego zapisu:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    string tekst = "Cześć, to jest przykładowy tekst.";
    cout << "Długość tekstu to: " << tekst.length() << endl;
    
    return 0;
}

/* Wynik:
Długość tekstu to: 31
*/
```

## Wgląd w to jak to działa
Aby zrozumieć w jaki sposób funkcje `length()` i `size()` działają, musimy zagłębić się w mechanizm stringa w C++. W przypadku typu `std::string`, mamy do czynienia z obiektem, który zawiera zarówno adres przechowujący dane, jak i informację o ich długości. W momencie wywołania funkcji `length()` lub `size()`, kompilator pobiera informację o długości zapisaną w obiekcie stringa i ją zwraca. Jest to bardzo wydajne i szybkie rozwiązanie, ponieważ funkcje te nie muszą przeliczać długości za każdym razem.

## Zobacz również
- [Dokumentacja C++: std::string](https://en.cppreference.com/w/cpp/string/basic_string)
- [Porównanie funkcji length() i size() w std::string](https://stackoverflow.com/questions/2848897/c-what-is-the-difference-between-size-and-length)