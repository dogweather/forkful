---
title:                "Wydobywanie podciągów"
html_title:           "Python: Wydobywanie podciągów"
simple_title:         "Wydobywanie podciągów"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Bardzo często w programowaniu musimy wyodrębnić cząstkowe ciągi z większych ciągów znaków. Na przykład, możemy potrzebować wyodrębnić część numeru telefonu lub imię z pełnego imienia i nazwiska. To jest dokładnie to, co robi funkcja `substring` — pozwala nam na wyodrębnienie pewnej części ciągu znaków.

## Jak to zrobić:

Torując drogę do użycia funkcji `substr` w C++, zwróćmy uwagę na poniższy kod:

```C++
#include <iostream>
#include <string>

int main()
{
    std::string fullname = "Jan Kowalski";
    std::string firstname = fullname.substr(0,3);
  
    std::cout << firstname << std::endl;

    return 0;
}
```

Po uruchomieniu powyższego kodu, wyjście będzie wyglądało tak:

```
Jan
```

W tym przypadku, `substr(0,3)` ekstrahuje 3 znaki, zaczynając od indeksu 0 (pierwszego znaku) z ciągu znaków `fullname`.

## Bardziej szczegółowo:

Funkcja `substr` jest częścią biblioteki `string` w C++ od początku jej istnienia, co pokazuje, jak ważne jest wyodrębnić podciągi.

A i o to, jak działa `substr`, polega na dwóch argumentach, które otrzymuje: początkowym indeksie oraz liczbie znaków, które chcemy wyodrębnić. C++ indeksuje ciągi znaków zaczynając od 0, więc `substr` również zaczyna od 0.

Co do alternatyw, można również użyć `std::find` i `std::find_if` w celu zlokalizowania początku i końca podciągu, a następnie użycie konstruktora `string`, który przyjmuje dwa iteratory.

## Zobacz także:

- [Biblioteka string na cppreference](https://en.cppreference.com/w/cpp/string/basic_string)
- [Dokumentacja substr na cppreference](https://en.cppreference.com/w/cpp/string/basic_string/substr)
- [Biblioteka algorithm na cppreference](https://en.cppreference.com/w/cpp/algorithm)