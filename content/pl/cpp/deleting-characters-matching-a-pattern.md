---
title:                "C++: Usuwanie znaków pasujących do wzoru"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego

Usuwanie znaków dopasowujących się do wzorca jest powszechnym wyzwaniem w programowaniu. Może być ono wykorzystane w wielu różnych sytuacjach, na przykład w celu usunięcia niechcianych znaków z tekstu lub w celu przetworzenia danych wejściowych.

## Jak to zrobić

Język C++ oferuje wiele różnych sposobów na usuwanie znaków dopasowujących się do wzorca. Jednym z nich jest użycie biblioteki standardowej "string" oraz funkcji "erase", która pozwala na usuwanie wybranych znaków z ciągu znaków. Poniżej znajduje się przykładowy kod, który pokazuje, jak można to zrobić:

```C++
#include <iostream>
#include <string>

int main() {
  // Przykładowy tekst z niechcianymi znakami
  std::string tekst = "To@jest%przykładowy&tekst#z niechcianymi znakami";
  
  // Usunięcie wszystkich znaków specjalnych
  for (int i = 0; i < tekst.length(); i++) {
    if (!isalpha(tekst[i])) { // Sprawdzenie, czy znak nie jest literą
      tekst.erase(i, 1); // Usunięcie znaku z tekstu
      i--; // Zmniejszenie indeksu, aby nie pominąć kolejnego znaku
    }
  }
  
  // Wyświetlenie przetworzonego tekstu
  std::cout << tekst << std::endl;
  
  return 0;
}

// Output:
// Tojestprzykładowytekstzniechcianymiznakami
```

## Zagłębienie

Istnieje wiele innych metod usuwania znaków dopasowujących się do wzorca, takich jak wykorzystanie wyrażeń regularnych lub funkcji z biblioteki "algorithm". Dzięki temu można w łatwy sposób dostosować kod do indywidualnych potrzeb.

## Zobacz również

- [Dokumentacja C++ o funkcji erase](https://www.cplusplus.com/reference/string/string/erase/)
- [Poradnik o wyrażeniach regularnych w C++](https://www.geeksforgeeks.org/regular-expression-in-c/)
- [Tutorial o funkcjach z biblioteki algorithm w języku C++](https://www.geeksforgeeks.org/algorithms-library-c-stl/)