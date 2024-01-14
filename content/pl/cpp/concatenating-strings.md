---
title:    "C++: Łączenie łańcuchów znaków"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

Jedną z podstawowych czynności w programowaniu jest łączenie lub łączenie danych tekstowych. To może wydawać się proste, ale w rzeczywistości jest to bardzo ważna umiejętność w wielu aplikacjach. W tym artykule dowiesz się, dlaczego łączenie łańcuchów jest niezbędne i jak to zrobić w języku C++.

## Jak to zrobić

Do łączenia łańcuchów w języku C++ używamy operatora plus (+). Oto przykładowy kod, który łączy dwa łańcuchy i wyświetla wynik na konsoli:

```C++
#include <iostream>
using namespace std;

int main() {
  string imie = "Anna";
  string nazwisko = "Kowalska";
  string pelne_imie = imie + " " + nazwisko;
  cout << pelne_imie;
  return 0;
}
```

W tym przykładzie stworzyliśmy trzy zmienne typu string: "imie", "nazwisko" i "pelne_imie". Następnie, używając operatora plus, połączyliśmy łańcuchy "imie" i "nazwisko" oraz dodaliśmy pomiędzy nimi spację. Na końcu program wyświetla wynik "Anna Kowalska" na ekranie.

## Deep Dive

Aby lepiej zrozumieć działanie operatora plus w kontekście łączenia łańcuchów, warto wiedzieć, że w języku C++ istnieje również klasa "string". Ta klasa zawiera wiele przydatnych funkcji do manipulowania i łączenia łańcuchów. Przykładem może być funkcja append(), która dodaje łańcuch na końcu innego łańcucha.

Inną przydatną rzeczą jest to, że w języku C++ można też łączyć zmienne innych typów (np. int czy double), zamieniając je na łańcuchy za pomocą funkcji to_string(). Dzięki temu jesteśmy w stanie łączyć różnego rodzaju dane w jeden łańcuch.

## Zobacz także

- [Dokumentacja C++ o operacjach na łańcuchach](https://docs.microsoft.com/pl-pl/cpp/standard-library/string-and-character-manipulation)
- [Tutorial C++ o łączeniu łańcuchów](https://www.tutorialspoint.com/cplusplus/cpp_strings_concatenation.htm)
- [Przydatne porady i triki dla początkujących programistów w C++](https://www.softwaretestinghelp.com/cpp-tips-tricks-and-interview-questions/)