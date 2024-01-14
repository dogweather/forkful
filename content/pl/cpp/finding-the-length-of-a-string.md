---
title:                "C++: Znajdowanie długości ciągu znaków"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego 

Często w programowaniu spotykamy się z potrzebą sprawdzenia długości ciągu znaków. Może to być przydatne w różnych sytuacjach, na przykład podczas pracy z tekstem lub przy walidacji danych wprowadzonych przez użytkownika. W tym blogu dowiesz się, jak w łatwy sposób znaleźć długość stringa w języku C++.

## Jak to zrobić 

W C++ długość stringa możemy sprawdzić za pomocą funkcji `length()` lub `size()`. Poniżej przedstawiamy przykładowy kod, który wykorzystuje obie te funkcje:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    // przykładowy string
    string imie = "Anna";
    
    // wyświetlenie długości za pomocą funkcji 'length()'
    cout << "Długość imienia: " << imie.length() << endl;
    
    // wyświetlenie długości za pomocą funkcji 'size()'
    cout << "Długość imienia: " << imie.size() << endl;
    
    return 0;
}
```

Output:
```
Długość imienia: 4
Długość imienia: 4
```

Jak widać, obie funkcje zwracają tę samą wartość, ponieważ są to dokładnie to samo.

## Głębszy zanurzenie 

W języku C++, string jest reprezentowany jako obiekt klasy `string`. Dlatego możemy wykorzystać metody tej klasy do manipulowania i sprawdzania długości tekstu. Funkcje `length()` i `size()` są metodami tej klasy, które zwracają liczbę znaków w stringu. Można więc powiedzieć, że te metody to tak naprawdę skrócone wersje metody `size()`.

Interesującym faktem jest również to, że w przeglądarce Google Chrome (opartej na języku C++) istnieje funkcja `strlen()` do sprawdzania długości ciągów znaków. Jest to implementacja funkcji `size()` i została dodana dla zgodności z językiem C. 

Warto również wiedzieć, że w języku C++ ciągi znaków mogą mieć różną długość. Nie są one ograniczone jak w innych językach, gdzie trzeba określić dokładną ilość znaków przy tworzeniu zmiennej typu string.

## Zobacz również 

- [Dokumentacja funkcji `length()`](https://www.cplusplus.com/reference/string/string/length/)
- [Dokumentacja funkcji `size()`](https://www.cplusplus.com/reference/string/string/size/)