---
title:                "Wyszukiwanie i zamiana tekstu."
html_title:           "C++: Wyszukiwanie i zamiana tekstu."
simple_title:         "Wyszukiwanie i zamiana tekstu."
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

Co to jest wyszukiwanie tekstu i jak zastępować go w programowaniu?

Wyszukiwanie i zastępowanie tekstu to podstawowa umiejętność każdego programisty. Pozwala ona na szybkie i efektywne modyfikowanie tekstu w kodzie, co jest niezbędne podczas pisania oprogramowania.

Jak tego dokonać w C++:

Przykład #1:
```C++
// Wyszukiwanie tekstu i zastępowanie go stałym wyrażeniem
#include <iostream>
#include <string>

int main() {
  std::string tekst = "Programowanie jest super!";
  tekst.replace(0, 12, "Tworzenie");
  std::cout << tekst;
  return 0;
}

// Output: Tworzenie jest super!
```

Przykład #2:
```C++
// Wyszukiwanie tekstu i zastępowanie go innym tekstem
#include <iostream>
#include <string>

int main() {
  std::string tekst = "To jest bardzo długi tekst, który potrzebuje skrócenia.";
  std::string tekst_szukany = "bardzo długi";
  std::string tekst_zastepczy = "krótki";
  
  size_t pozycja = tekst.find(tekst_szukany);
  if (pozycja != std::string::npos) {
    tekst.replace(pozycja, tekst_szukany.length(), tekst_zastepczy);
  }
  std::cout << tekst;
  return 0;
}

// Output: To jest krótki tekst, który potrzebuje skrócenia.
```

Głębszy rozkład:

Wyszukiwanie i zastępowanie tekstu jest bardzo popularną i przydatną operacją w programowaniu. W poprzednich latach, kiedy programowanie było bardziej zorientowane na tekst, często musieliśmy dokonywać tego ręcznie, używając edytora tekstu lub skryptów. Teraz, dzięki zaawansowanym funkcjom języków programowania, takim jak C++, możemy dokładnie określić, co chcemy znaleźć i do czego chcemy to zastąpić.

Alternatywy do przeszukiwania tekstu i jego zastępowania obejmują ręczne edycje i użycie bardziej złożonych narzędzi i skryptów. Jednak takie podejście może zajmować więcej czasu i być mniej precyzyjne.

W C++, używamy metody "replace" z klasy "string" do zastępowania tekstu. Funkcja ta przyjmuje dwa lub trzy argumenty: pierwszy argument to pozycja, od której chcemy rozpocząć zastępowanie, drugi argument to długość tekstu, który chcemy zastąpić, a trzeci argument (opcjonalny) to tekst, którym chcemy zastąpić wybrany fragment. Aby dokładnie określić, co chcemy znaleźć, używamy funkcji "find", która zwraca pozycję znajdującego się w tekście wyrażenia. Jeśli chcemy zastąpić jedynie pierwsze wystąpienie danego wyrażenia, możemy odwołać się do funkcji "find_first_of", a jeśli wymagamy zastąpienia wszystkich wystąpień, używamy funkcji "find_all_of".

Zobacz też: 
- [Dokumentacja języka C++ na temat funkcji replace](https://en.cppreference.com/w/cpp/string/basic_string/replace)
- [Inne przydatne funkcje do operacji na tekstach w C++](https://www.geeksforgeeks.org/c-string-class-and-its-applications/)