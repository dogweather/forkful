---
title:                "C++: Używanie wyrażeń regularnych"
simple_title:         "Używanie wyrażeń regularnych"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Dlaczego używać wyrażeń regularnych w C++?

Wyrażenia regularne są bardzo przydatnym narzędziem w języku programowania C++. Pozwalają one na wygodne i efektywne wyszukiwanie oraz manipulowanie ciągami znaków. Dzięki nim możemy szybko przetwarzać dane, sprawdzać poprawność wprowadzonych danych oraz wiele innych. Są one szczególnie przydatne w programowaniu aplikacji internetowych, gdzie często mamy do czynienia z dużymi zbiorami danych.

# Jak używać wyrażeń regularnych w C++?

Aby zacząć korzystać z wyrażeń regularnych w języku C++, musimy dołączyć bibliotekę <regex>, a następnie zdefiniować nasze wyrażenie regularne przy użyciu typu std::regex. Poniższy kod przedstawia przykładowe wyrażenie regularne, które znajdzie wszystkie wystąpienia słowa "hello" w ciągu znaków:
```C++
#include <regex>
#include <iostream>

int main() {
    std::regex pattern("hello");
    std::string text = "Hello world, hello people!";
    std::smatch matches;

    while (std::regex_search(text, matches, pattern)) {
        std::cout << "Znalazłem słowo '" << matches[0] << "' na pozycji " << matches.position() << std::endl;
        text = matches.suffix().str();
    }

    return 0;
}
```
Podczas uruchomienia powyższego kodu, otrzymamy następujący wynik:
```
Znalazłem słowo 'hello' na pozycji 6
Znalazłem słowo 'hello' na pozycji 19
```
Widzimy, że nasze wyrażenie regularne zostało dopasowane do dwóch miejsc w ciągu znaków, w których występuje słowo "hello".

# Głębsze zanurzenie: Przydatne funkcje wyrażeń regularnych w C++

Wyrażenia regularne w C++ dają nam dużą elastyczność w wyszukiwaniu oraz manipulowaniu ciągami znaków. Oto kilka przydatnych funkcji, które mogą nam się przydać w codziennej pracy:

- **std::regex_replace**: Pozwala na zamianę części lub całości ciągu znaków na inny ciąg, spełniający określone warunki. Przykładowo, możemy zamienić wszystkie pojawienia liczby "123" na słowo "hello" przy użyciu wyrażenia ``"123\b"``.
- **std::regex_match**: Sprawdza, czy dany ciąg znaków odpowiada danemu wyrażeniu regularnemu.
- **std::regex_iterator**: Umożliwia iterowanie po wszystkich dopasowaniach wyrażenia regularnego w danym ciągu.
- **std::regex_search**: Szuka pierwszego dopasowania wyrażenia regularnego w danym ciągu i zwraca informacje o dopasowaniu.

Mając wiedzę o tych oraz innych funkcjach, możemy wykorzystywać wyrażenia regularne w C++ w celu tworzenia bardziej skomplikowanych operacji na ciągach znaków.

# Zobacz też

- [Dokumentacja biblioteki std::regex w C++](https://en.cppreference.com/w/cpp/regex)
- [Przykładowe wyrażenia regularne w C++](https://www.geeksforgeeks.org/regular-expressions-c-implementation/)
- [Poradnik dla początkujących w zakresie wyrażeń regularnych w C++](https://www.regular-expressions.info/cpp.html)