---
title:    "C++: Konwertowanie daty na ciąg znaków"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwersja daty na ciąg znaków jest niezbędnym procesem w wielu aplikacjach, gdyż pomaga w wyświetlaniu czytelnego formatu daty dla użytkowników. Jest to również często wymagane przy przetwarzaniu danych wejściowych lub wyświetlaniu raportów. W tym blogu omówimy różne sposoby konwersji daty na string w języku C++.

## Jak to zrobić

 Istnieje wiele sposobów na dokonanie konwersji daty na ciąg znaków, ale w tym artykule skupimy się na dwóch najczęściej wykorzystywanych metodach: wykorzystanie biblioteki <ctime> oraz wykorzystanie biblioteki Boost.

Pierwsza metoda polega na wykorzystaniu funkcji z biblioteki <ctime> do zamiany daty na wartość typu time_t, a następnie wykorzystaniu funkcji strftime (znowu dostępnej w <ctime>) do sformatowania wartości time_t do wybranego przez nas formatu daty.

```c++
#include <ctime> 
#include <iostream> 

int main() { 
    time_t now = time(0); 
    char buffer[80]; 
    strftime(buffer, 80, "%d/%m/%Y", localtime(&now)); 
    std::cout << buffer << std::endl; 
}
```

Output: ```18/10/2021``` 

Drugą metodą będzie wykorzystanie biblioteki Boost, która posiada wiele specjalistycznych narzędzi, w tym również do konwersji daty na string. Przy użyciu funkcji to_iso_extended_string z biblioteki Boost, można wygodnie skonwertować datę do wybranego przez nas formatu.

```c++
#include <boost/date_time.hpp> 
#include <iostream> 

int main() { 
    boost::gregorian::date date(boost::gregorian::day_clock::local_day()); 
    std::string date_string = boost::gregorian::to_iso_extended_string(date); 
    std::cout << date_string << std::endl; 
}
```

Output: ```2021-10-18``` 

## Deep Dive

Konwersja daty na ciąg znaków może wydawać się prostym zadaniem, ale w rzeczywistości jest to proces wymagający od programisty dokładnej znajomości bibliotek i funkcji języka C++. W przypadku bardziej złożonych formatów daty, trzeba również brać pod uwagę ustawienia regionalne i różnice w sposobie wyświetlania dat dla różnych kultur.

## Zobacz też

- [Przewodnik po bibliotece C++ <ctime>](https://pl.cppreference.com/w/cpp/header/ctime)
- [Dokumentacja biblioteki Boost dla języka C++](https://www.boost.org/doc/libs/1_77_0/)