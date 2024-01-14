---
title:                "C++: Ekstrakcja podciągów"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego

Aby zapewnić swoim programom większą elastyczność i funkcjonalność, często konieczne jest wyodrębnienie części tekstu z ciągów znaków. Jest to szczególnie przydatne w przypadku analizy i przetwarzania danych tekstowych. W tym artykule dowiesz się, jak wyodrębnić podciągi w języku C++, aby ułatwić sobie pracę z tekstem.

## Jak to zrobić

W języku C++ istnieje wiele sposobów na wyodrębnienie podciągów. Jednym z nich jest użycie funkcji ```substr()```, która pozwala na podanie początkowego indeksu i długości podciągu, który chcemy wyodrębnić. Na przykład:

```
string str = "To jest przykładowy tekst";
string substr = str.substr(8, 11);

cout << substr << endl;
// wyświetli "przykładowy"
```

Inną metodą jest użycie iteratorów i specjalnego konstruktora ```string()``` do przekształcenia podciągu w nowy obiekt typu string. Na przykład:

```
string str = "To jest przykładowy tekst";
string substr(str.begin() + 8, str.begin() + 20);

cout << substr << endl;
// wyświetli "przykładowy tekst"
```

W obu przypadkach można również wykorzystać funkcje wbudowane, takie jak ```find()``` do znalezienia indeksu pierwszego lub ostatniego wystąpienia danego znaku lub ciągu znaków. Dzięki temu możliwe jest elastyczne wyodrębnianie podciągów o różnych długościach.

## Zagłębiona analiza

Podciągi są wykorzystywane w wielu popularnych algorytmach przetwarzania tekstu, takich jak wyszukiwanie wzorca i sortowanie. Mogą być również wykorzystywane do przekształcania danych tekstowych w odpowiedni format lub do wyodrębniania ważnych informacji z tekstów źródłowych. Wykorzystanie funkcji do wyodrębniania podciągów w języku C++ jest więc ważną umiejętnością dla każdego programisty.

## Zobacz też

- [Dokumentacja funkcji substr() w języku C++](https://en.cppreference.com/w/cpp/string/basic_string/substr)
- [Przykłady wykorzystania funkcji find() do wyodrębniania podciągów](https://www.geeksforgeeks.org/string-find-in-cpp/)
- [Poradnik dla początkujących: Podstawy manipulacji ciągami znaków w C++](https://www.tutorialspoint.com/cplusplus/cpp_strings.htm)